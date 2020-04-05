(uiop:define-package :hindley-milner/typecheck/infer
    (:nicknames :infer)
  (:mix
   :hindley-milner/ir1
   :hindley-milner/typecheck/substitute
   :iterate
   :cl)
  (:import-from :trivial-types
   :association-list)
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

(declaim (ftype (function (constraint) type)
                constraint-lhs))


(defgeneric infer (expr type-env)
  (:documentation "returns (`VALUES' TYPED-IR1 TYPE CONSTRAINTS)

where:
TYPED-IR1 is a new node which is like EXPR but with type annotations,
TYPE is the inferred type of EXPR, and
CONSTRAINTS is an (`ASSOCIATION-LIST' `TYPE' `TYPE') denoting the constraints to solve"))

(defmethod infer ((expr variable) type-env)
  (let* ((name (variable-name expr))
         (type (instantiate (type-env-lookup type-env name)))
         (new-node (make-instance 'variable
                                  :type type
                                  :name name)))
    (values new-node type ())))

(defmethod infer ((expr funcall) type-env)
  (multiple-value-bind (func-node func-type func-constraints)
      (infer (funcall-function expr) type-env)
    (iter
      (for arg-expr in-vector (funcall-args expr))
      (for (values arg-node arg-type arg-constraint) = (infer arg-expr type-env))
      (collect arg-node into arg-nodes result-type (vector expr))
      (collect arg-type into arg-types result-type (vector type))
      (unioning arg-constraint into arg-constraints)
      (finally
       (let* ((return-type (new-type-variable))
             (arrow-type (make-instance '->
                                        :inputs arg-types
                                        :output return-type))
             (funcall-constraints (acons arrow-type func-type ()))
             (all-constraints (append funcall-constraints func-constraints arg-constraints))
             (new-node (make-instance 'funcall
                                      :type return-type
                                      :function func-node
                                      :args arg-nodes)))
         (return (values new-node
                         return-type
                         all-constraints)))))))

(defun make-constant-scheme (body)
  (make-instance 'type-scheme
                 :bindings ()
                 :body body))

(defmethod infer ((expr lambda) type-env)
  (iter
    (with function-env = type-env)
    (for binding in-vector (lambda-bindings expr))
    (for arg-type = (new-type-variable binding))
    (for scheme = (make-constant-scheme arg-type))
    (collect arg-type into arg-types result-type (vector type))
    (push (cons binding scheme) function-env)
    (finally
     (multiple-value-bind (body return-type constraints)
        (infer (lambda-body expr) function-env)
      (let* ((arrow-type (make-instance '->
                                       :inputs arg-types
                                       :output return-type))
             (new-node (make-instance 'lambda
                                      :type arrow-type
                                      :bindings (lambda-bindings expr)
                                      :body body)))
        (return (values new-node
                        arrow-type
                        constraints)))))))

(defmethod infer ((expr poly-let) type-env)
  (multiple-value-bind (init-expr value-type value-constraints)
      (infer (poly-let-initform expr) type-env)
    (let* ((value-scheme (generalize value-type type-env))
           (binding (poly-let-binding expr))
           (local-env (acons binding value-scheme type-env)))
      (multiple-value-bind (body return-type body-constraints)
          (infer (poly-let-body expr) local-env)
        (values (make-instance 'poly-let
                               :type return-type
                               :binding binding
                               :scheme value-scheme
                               :initform init-expr
                               :body body)
                return-type
                (append value-constraints body-constraints))))))

(defmethod infer ((expr if) type-env)
  (multiple-value-bind (pred-expr pred-type pred-constraints)
      (infer (if-predicate expr) type-env)
    (multiple-value-bind (then-expr then-type then-constraints)
        (infer (if-then-case expr) type-env)
      (multiple-value-bind (else-expr else-type else-constraints)
          (infer (if-else-case expr) type-env)
        (let* ((new-node (make-instance 'if
                                        :type then-type
                                        :predicate pred-expr
                                        :then-case then-expr
                                        :else-case else-expr))
               (if-constraints (acons pred-type *boolean*
                                      (acons then-type else-type ())))
               (all-constraints
                 (append if-constraints pred-constraints then-constraints else-constraints)))
          (values new-node
                  then-type
                  all-constraints))))))

(defmethod infer ((expr prog2) type-env)
  (multiple-value-bind (side-effect-expr side-effect-type side-effect-constraints)
      (infer (prog2-side-effect expr) type-env)
    (declare (ignore side-effect-type))
    (multiple-value-bind (return-expr return-type return-constraints)
        (infer (prog2-return-value expr) type-env)
      (values (make-instance 'prog2
                             :type return-type
                             :side-effect side-effect-expr
                             :return-value return-expr)
              return-type
              (append side-effect-constraints return-constraints)))))

(defmethod infer ((expr quote) type-env)
  (declare (ignorable type-env))
  (let ((type (etypecase (quote-it expr)
                (boolean *boolean*)
                (fixnum *fixnum*))))
    (values (make-instance 'quote
                           :type type
                           :it (quote-it expr))
            type
            ())))
