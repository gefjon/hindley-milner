(uiop:define-package :hindley-milner/typecheck/infer
    (:nicknames :infer)
  (:mix :hindley-milner/typecheck/type
   :hindley-milner/typecheck/substitute
        :trivial-types :iterate :cl)
  (:import-from :hindley-milner/ir1)
  (:import-from :hindley-milner/typecheck/typed-ir1)
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
  (:documentation "returns (`VALUES' TYPED-IR1 TYPE CONSTRAINTS)

where:
TYPED-IR1 is a new node which is like EXPR but with type annotations,
TYPE is the inferred type of EXPR, and
CONSTRAINTS is an (`ASSOCIATION-LIST' `TYPE' `TYPE') denoting the constraints to solve"))

(defmethod infer ((expr ir1:variable) type-env)
  (let* ((name (ir1:variable-name expr))
         (type (instantiate (type-env-lookup type-env name)))
         (new-node (typed-ir1:make-variable type name)))
    (values new-node type ())))

(defmethod infer ((expr ir1:funcall) type-env)
  (multiple-value-bind (func-node func-type func-constraints)
      (infer (ir1:funcall-function expr) type-env)
    (multiple-value-bind (arg-node arg-type arg-constraints)
        (infer (ir1:funcall-arg expr) type-env)
      (let* ((return-type (new-type-variable))
             (arrow-type (make--> arg-type return-type))
             (funcall-constraints (acons arrow-type func-type ()))
             (all-constraints (append funcall-constraints func-constraints arg-constraints))
             (new-node (typed-ir1:make-funcall return-type func-node arg-node)))
        (values new-node
                return-type
                all-constraints)))))

(defmethod infer ((expr ir1:lambda) type-env)
  (let* ((binding (ir1:lambda-binding expr))
         (arg-type (new-type-variable binding))
         (function-env (acons binding (make-type-scheme () arg-type) type-env)))
    (multiple-value-bind (body return-type constraints)
        (infer (ir1:lambda-body expr) function-env)
      (let ((arrow-type (make--> arg-type return-type)))
        (values (typed-ir1:make-lambda arrow-type binding body)
                arrow-type
                constraints)))))

(defmethod infer ((expr ir1:let) type-env)
  (multiple-value-bind (init-expr value-type value-constraints)
      (infer (ir1:let-initform expr) type-env)
    (let* ((value-scheme (generalize value-type type-env))
           (binding (ir1:let-binding expr))
           (local-env (acons binding value-scheme type-env)))
      (multiple-value-bind (body return-type body-constraints)
          (infer (ir1:let-body expr) local-env)
        (values (typed-ir1:make-let return-type binding value-scheme init-expr body)
                return-type
                (append value-constraints body-constraints))))))

(defmethod infer ((expr ir1:if) type-env)
  (multiple-value-bind (pred-expr pred-type pred-constraints)
      (infer (ir1:if-predicate expr) type-env)
    (multiple-value-bind (then-expr then-type then-constraints)
        (infer (ir1:if-then-case expr) type-env)
      (multiple-value-bind (else-expr else-type else-constraints)
          (infer (ir1:if-else-case expr) type-env)
        (let* ((new-node (typed-ir1:make-if then-type pred-expr then-expr else-expr))
               (if-constraints (acons pred-type type:*boolean*
                                      (acons then-type else-type ())))
               (all-constraints
                 (append if-constraints pred-constraints then-constraints else-constraints)))
          (values new-node
                  then-type
                  all-constraints))))))

(defmethod infer ((expr ir1:prog2) type-env)
  (multiple-value-bind (side-effect-expr side-effect-type side-effect-constraints)
      (infer (ir1:prog2-side-effect expr) type-env)
    (declare (ignore side-effect-type))
    (multiple-value-bind (return-expr return-type return-constraints)
        (infer (ir1:prog2-return-value expr) type-env)
      (values (typed-ir1:make-prog2 return-type side-effect-expr return-expr)
              return-type
              (append side-effect-constraints return-constraints)))))

(defmethod infer ((expr ir1:quote) type-env)
  (declare (ignorable type-env))
  (let ((type (etypecase (ir1:quote-it expr)
                (boolean type:*boolean*)
                (fixnum type:*fixnum*))))
    (values (typed-ir1:make-quote type (ir1:quote-it expr))
            type
            ())))
