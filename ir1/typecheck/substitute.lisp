(uiop:define-package :hindley-milner/ir1/typecheck/substitute
  (:import-from :trivial-types
   :association-list)
  (:mix
   :hindley-milner/prologue
   :hindley-milner/subst
   :hindley-milner/ir1/type
   :cl
   :iterate)
  (:export :substitution :apply-substitution :instantiate :generalize))
(cl:in-package :hindley-milner/ir1/typecheck/substitute)

(deftype substitution ()
  '(association-list type-variable type))

(defun apply-substitution (substitution target)
  (if substitution
      (let* ((this-subst (first substitution))
             (old (car this-subst))
             (new (cdr this-subst))
             (rest-subst (subst new old (rest substitution)))
             (new-tree (subst new old target)))
        (apply-substitution rest-subst new-tree))
      target))

(|:| #'instantiate (-> (type-scheme) type))
(defun instantiate (type-scheme)
  (flet ((make-substitution-cell (type-var)
           (cons type-var (new-type-variable type-var))))
    (apply-substitution (mapcar #'make-substitution-cell (bindings type-scheme))
                        (body type-scheme))))

(defgeneric free-type-variables (within)
  (:documentation "returns a set of type-variables"))

(defmethod free-type-variables ((within type-scheme))
  (set-difference (free-type-variables (body within))
                  (bindings within)))

(defmethod free-type-variables ((within type-variable))
  (list within))

(defmethod free-type-variables ((within type-primitive))
  (declare (ignorable within))
  '())

(defmethod free-type-variables ((within vector))
  (iter
    (for element in-vector within)
    (adjoining element)))

(defmethod free-type-variables ((within arrow))
  (union (free-type-variables (inputs within))
         (free-type-variables (output within))))

;; this is a named function (rather than a method on
;; `FREE-TYPE-VARIABLES') because `TYPE-ENV' names a type rather than
;; a class. `TYPE-ENV's are just alists, and can be operated on as
;; such.
(defun type-env-free-variables (type-env)
  (iter (for (key . value) in type-env)
    (declare (ignorable key))
    (unioning (free-type-variables value))))

(|:| #'generalize (-> (type type-env) type-scheme))
(defun generalize (type type-env)
  (make-instance 'type-scheme
                 :bindings (set-difference (free-type-variables type)
                                           (type-env-free-variables type-env))
                 :body type))
