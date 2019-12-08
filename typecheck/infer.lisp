(uiop:define-package :hindley-milner/typecheck/infer
    (:nicknames :infer)
  (:mix :hindley-milner/typecheck/type
   :hindley-milner/typecheck/substitute
        :trivial-types :iterate :cl)
  (:import-from :hindley-milner/ir1)
  (:export :infer :constraint :constraints :constraint-lhs :constraint-rhs))
(cl:in-package :hindley-milner/typecheck/infer)

(deftype constraint ()
  '(cons type type))

(declaim (ftype (function (constraint) type)
                constraint-lhs))
(defun constraint-lhs (constraint)
  (car constraint))

(declaim (ftype (function (constraint) type)
                constraint-rhs))
(defun constraint-rhs (constraint)
  (cdr constraint))

(deftype constraints ()
  '(association-list type type))

(declaim (ftype (function (type type constraints) constraints)
                constraint-acons))
(defun constraint-acons (lhs rhs tail)
  (acons lhs rhs tail))

(declaim (ftype (function (constraint) type)
                constraint-lhs))


(declaim (ftype (function (type-env symbol) type-scheme)
                type-env-lookup))
(defun type-env-lookup (type-env symbol)
  (or (cdr (assoc symbol type-env))
      (error "type-variable ~s unbound in type-env ~s" symbol type-env)))

(defgeneric infer (expr type-env)
  (:documentation "returns (`VALUES' TYPE CONSTRAINTS), where TYPE is the inferred type of EXPR, and CONSTRAINTS is an (`ASSOCIATION-LIST' `TYPE' `TYPE') denoting the constraints to solve"))

(defmethod infer :around ((expr ir1:typed-node) type-env)
  (if (ir1:type-already-computed-p expr)
      (ir1:typed-node-type expr)
      (multiple-value-bind (type constraints) (call-next-method)
        (setf (ir1:typed-node-type expr) type)
        (values type constraints))))

(defmethod infer ((expr ir1:variable) type-env)
  (instantiate (type-env-lookup type-env (ir1:variable-name expr))))

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
      (setf (ir1:let-scheme expr) value-scheme
            (ir1:let-internal-env expr) local-env)
      (multiple-value-bind (return-type body-constraints)
          (infer (ir1:let-body expr) local-env)
        (values return-type
                (append value-constraints body-constraints))))))

(defmethod infer ((expr ir1:if) type-env)
  (multiple-value-bind (pred-type pred-constraints) (infer (ir1:if-predicate expr) type-env)
    (multiple-value-bind (then-type then-constraints) (infer (ir1:if-then-case expr) type-env)
      (multiple-value-bind (else-type else-constraints) (infer (ir1:if-else-case expr) type-env)
        (let ((if-constraints (acons pred-type type:*boolean*
                                     (acons then-type else-type ()))))
          (values then-type
                  (append if-constraints pred-constraints then-constraints else-constraints)))))))

(defmethod infer ((expr ir1:prog2) type-env)
  (multiple-value-bind (side-effect-type side-effect-constraints)
      (infer (ir1:prog2-side-effect expr) type-env)
    (declare (ignore side-effect-type))
    (multiple-value-bind (return-type return-constraints)
        (infer (ir1:prog2-return-value expr) type-env)
      (values return-type (append side-effect-constraints return-constraints)))))

(defmethod infer ((expr ir1:quote) type-env)
  (declare (ignorable type-env))
  (values 
   (etypecase (ir1:quote-it expr)
     (boolean type:*boolean*)
     (fixnum type:*fixnum*))
   '()))
