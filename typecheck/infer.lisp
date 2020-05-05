(uiop:define-package :hindley-milner/typecheck/infer
    (:nicknames :infer)
  (:mix
   :hindley-milner/ir1
   :hindley-milner/typecheck/substitute
   :hindley-milner/prologue
   :iterate
   :cl)
  (:import-from :trivial-types
   :association-list)
  (:import-from :hindley-milner/ir1)
  (:export
   :infer-program
   :constraint :constraints :constraint-lhs :constraint-rhs))
(cl:in-package :hindley-milner/typecheck/infer)

(deftype constraint ()
  '(cons type type))

(|:| #'constraint-lhs (-> (constraint) type))
(defun constraint-lhs (constraint)
  (car constraint))

(|:| #'constraint-rhs (-> (constraint) type))
(defun constraint-rhs (constraint)
  (cdr constraint))

(deftype constraints ()
  '(association-list type type))

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
             (arrow-type (make-instance 'arrow
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
      (let* ((arrow-type (make-instance 'arrow
                                       :inputs arg-types
                                       :output return-type))
             (new-node (make-instance 'lambda
                                      :type arrow-type
                                      :bindings (lambda-bindings expr)
                                      :body body)))
        (return (values new-node
                        arrow-type
                        constraints)))))))

(|:| #'extend-type-env-for-def (-> (polymorphic type-env) type-env))
(defun extend-type-env-for-def (def type-env)
  (acons (definition-name def) (polymorphic-scheme def)
         type-env))

(|:| #'infer-definition (-> (untyped type-env) (values polymorphic constraints type-env &optional)))
(defun infer-definition (untyped type-env)
  "add type information to an `UNTYPED' `DEFINITION', returning three values:

a `POLYMORPHIC' `DEFINITION', a `CONSTRAINTS' object describing the
requirements on that definition, and a `TYPE-ENV' that extends
TYPE-ENV with information about the new definition."
  (multiple-value-bind (new-initform type constraints)
      (infer (definition-initform untyped) type-env)
    (let* ((new-def (make-instance 'polymorphic
                                   :name (definition-name untyped)
                                   :initform new-initform
                                   :scheme (generalize type type-env))))
      (values new-def
              constraints
              (extend-type-env-for-def new-def type-env)))))

(defmethod infer ((expr let) type-env)
  (multiple-value-bind (new-def def-constraints local-env)
      (infer-definition (let-def expr) type-env)
    (multiple-value-bind (new-body return-type body-constraints)
        (infer (let-body expr) local-env)
      (values (make-instance 'let
                             :type return-type
                             :def new-def
                             :body new-body)
              return-type
              (append def-constraints body-constraints)))))

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
  (let* ((type (etypecase (quote-it expr)
                (boolean *boolean*)
                (fixnum *fixnum*))))
    (values (make-instance 'quote
                           :type type
                           :it (quote-it expr))
            type
            ())))

(|:| #'infer-program (-> (program) (values program constraints &optional)))
(defmethod infer-program (program)
  (iter
    (with type-env = ())
    (for def in-vector (program-definitions program))
    (for (values new-def def-constraints new-env) = (infer-definition def type-env))
    (setf type-env new-env)
    (appending def-constraints into constraints)
    (collect new-def into definitions result-type (vector definition))
    (finally
     (multiple-value-bind (entry result-type entry-constraints)
         (infer (program-entry program) type-env)
       (declare (ignore result-type))
       (return
         (values
          (make-instance 'program
                         :definitions definitions
                         :entry entry)
          (append entry-constraints constraints)))))))

