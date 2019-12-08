(uiop:define-package :hindley-milner/typecheck/substitute
    (:nicknames :substitute)
  (:mix :hindley-milner/typecheck/type :trivial-types :cl :iterate)
  (:import-from :alexandria)
  (:import-from :hindley-milner/ir1)
  (:export :substitution :apply-substitution :instantiate :generalize))
(cl:in-package :hindley-milner/typecheck/substitute)

(deftype substitution ()
  '(association-list symbol type))

(defgeneric apply-substitution (substitution target))

(defmethod apply-substitution (substitution (target symbol))
  (alexandria:if-let (assoc (assoc target substitution))
    (cdr assoc)
    target))

(defmethod apply-substitution (substitution (target type-primitive))
  (declare (ignorable substitution))
  target)

(defmethod apply-substitution (substitution (target ->))
  (flet ((recurse (target) (apply-substitution substitution target)))
    (make--> (recurse (->-input target))
             (recurse (->-output target)))))

(defmethod apply-substitution (substitution (target cons))
  (flet ((recurse (target) (apply-substitution substitution target)))
    (cons (recurse (car target))
          (recurse (cdr target)))))

(defmethod apply-substitution (substitution (target null))
  (declare (ignorable substitution))
  target)

(declaim (ftype (function (type-scheme) type)
                instantiate))
(defun instantiate (type-scheme)
  (flet ((make-substitution-cell (type-var)
           (cons type-var (gensym (symbol-name type-var)))))
    (apply-substitution (mapcar #'make-substitution-cell (type-scheme-bindings type-scheme))
                        (type-scheme-body type-scheme))))

(defgeneric free-type-variables (within)
  (:documentation "returns a set of symbols"))

(defmethod free-type-variables ((within type-scheme))
  (set-difference (free-type-variables (type-scheme-body within))
                  (type-scheme-bindings within)))

(defmethod free-type-variables ((within symbol))
  (list within))

(defmethod free-type-variables ((within type-primitive))
  (declare (ignorable within))
  '())

(defmethod free-type-variables ((within ->))
  (union (free-type-variables (->-input within))
         (free-type-variables (->-output within))))

;; this is a named function (rather than a method on
;; `FREE-TYPE-VARIABLES') because `TYPE-ENV' names a type rather than
;; a class. `TYPE-ENV's are just alists, and can be operated on as
;; such.
(defun type-env-free-variables (type-env)
  (iter (for (key . value) in type-env)
    (unioning (free-type-variables value))))

(declaim (ftype (function (type type-env) type-scheme)
                generalize))
(defun generalize (type type-env)
  (make-type-scheme (set-difference (free-type-variables type)
                                    (type-env-free-variables type-env))
                    type))
