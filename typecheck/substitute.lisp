(uiop:define-package :hindley-milner/typecheck/substitute
    (:nicknames :substitute)
  (:mix :hindley-milner/subst :hindley-milner/ir1/type :trivial-types :cl :iterate)
  (:import-from :alexandria)
  (:export :substitution :apply-substitution :instantiate :generalize))
(cl:in-package :hindley-milner/typecheck/substitute)

(deftype substitution ()
  '(association-list type-variable type))

(defun apply-substitution (substitution target)
  (iter (with tree = target)
    (for (old . new) in substitution)
    (setf tree (subst new old tree))
    (finally (return tree))))

(declaim (ftype (function (type-scheme) (values type &optional))
                instantiate))
(defun instantiate (type-scheme)
  (flet ((make-substitution-cell (type-var)
           (cons type-var (new-type-variable type-var))))
    (apply-substitution (mapcar #'make-substitution-cell (type-scheme-bindings type-scheme))
                        (type-scheme-body type-scheme))))

(defgeneric free-type-variables (within)
  (:documentation "returns a set of type-variables"))

(defmethod free-type-variables ((within type-scheme))
  (set-difference (free-type-variables (type-scheme-body within))
                  (type-scheme-bindings within)))

(defmethod free-type-variables ((within type-variable))
  (list within))

(defmethod free-type-variables ((within type-primitive))
  (declare (ignorable within))
  '())

(defmethod free-type-variables ((within vector))
  (iter
    (for element in-vector within)
    (adjoining element)))

(defmethod free-type-variables ((within ->))
  (union (free-type-variables (->-inputs within))
         (free-type-variables (->-output within))))

;; this is a named function (rather than a method on
;; `FREE-TYPE-VARIABLES') because `TYPE-ENV' names a type rather than
;; a class. `TYPE-ENV's are just alists, and can be operated on as
;; such.
(defun type-env-free-variables (type-env)
  (iter (for (key . value) in type-env)
    (unioning (free-type-variables value))))

(declaim (ftype (function (type type-env) (values type-scheme &optional))
                generalize))
(defun generalize (type type-env)
  (make-instance 'type-scheme
                 :bindings (set-difference (free-type-variables type)
                                           (type-env-free-variables type-env))
                 :body type))
