(uiop:define-package :hindley-milner/typecheck/infer
    (:nicknames :infer)
  (:mix :hindley-milner/typecheck/type
   :hindley-milner/typecheck/substitute
        :trivial-types :iterate :cl)
  (:import-from :hindley-milner/ir1))
(cl:in-package :hindley-milner/typecheck/infer)

(deftype constraint ()
  '(cons type type))

(deftype constraints ()
  '(association-list type type))

(declaim (ftype (function (type-env symbol) type-scheme)
                type-env-lookup))
(defun type-env-lookup (type-env symbol)
  (or (cdr (assoc symbol type-env))
      (error "type-variable ~s unbound in type-env ~s" symbol type-env)))

(defgeneric infer (expr type-env)
  (:documentation "returns (`VALUES' TYPE CONSTRAINTS), where TYPE is the inferred type of EXPR, and CONSTRAINTS is an (`ASSOCIATION-LIST' `TYPE' `TYPE') denoting the constraints to solve"))

(defmethod infer ((expr symbol) type-env)
  (instantiate (type-env-lookup type-env expr)))

(defmethod infer ((expr ir1:funcall) type-env)
  (multiple-value-bind (func-type func-constraints) (infer (ir1:funcall-function expr)
                                                           type-env)
    (multiple-value-bind (arg-type arg-constraints) (infer (ir1:funcall-arg expr)
                                                           type-env)
      (let* ((return-type (new-type-variable))
             (arrow-type (make--> arg-type return-type))
             (funcall-constraints (acons arrow-type func-type ()))
             (all-constraints (append funcall-constraints func-constraints arg-constraints)))
        (values return-type
                all-constraints)))))

(defmethod infer ((expr ir1:lambda) type-env)
  (let* ((arg-type (new-type-variable (ir1:lambda-binding expr)))
         (function-env (acons (ir1:lambda-binding expr) (make-type-scheme () arg-type) type-env)))
    (multiple-value-bind (return-type constraints) (infer (ir1:lambda-body expr) function-env)
      (values (make--> arg-type return-type)
              constraints))))

(defmethod infer ((expr ir1:let) type-env)
  (multiple-value-bind (value-type value-constraints) (infer (ir1:let-initform expr) type-env)
    (let* ((value-scheme (generalize value-type type-env))
           (local-env (acons (ir1:let-binding expr) value-scheme type-env)))
      (multiple-value-bind (return-type body-constraints)
          (infer (ir1:let-body expr) local-env)
        (values return-type
                (append value-constraints body-constraints))))))

(defmethod infer ((expr ir1:if) type-env)
  (multiple-value-bind (pred-type pred-constraints) (infer (ir1:if-predicate expr) type-env)
    (multiple-value-bind (then-type then-constraints) (infer (ir1:if-then-case expr) type-env)
      (multiple-value-bind (else-type else-constraints) (infer (ir1:if-else-case expr) type-env)
        (let ((if-constraints (acons pred-type 'hm:|boolean|
                                     (acons then-type else-type ()))))
          (values then-type
                  (append if-constraints pred-constraints then-constraints else-constraints)))))))
