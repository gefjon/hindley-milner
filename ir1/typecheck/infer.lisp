(uiop:define-package :hindley-milner/ir1/typecheck/infer
  (:mix
   :hindley-milner/ir1/expr
   :hindley-milner/ir1/typecheck/substitute
   :hindley-milner/prologue
   :iterate
   :cl)
  (:import-from :trivial-types
   :proper-list)
  (:export
   :infer-program
   :constraint :eqv :constraints :lhs :rhs))
(cl:in-package :hindley-milner/ir1/typecheck/infer)

(define-enum constraint ()
  ((eqv ((lhs type)
         (rhs type)))))

(|:| #'eqv (-> (type type) eqv))
(defun eqv (lhs rhs)
  (make-instance 'eqv :lhs lhs :rhs rhs))

(deftype constraints ()
  '(proper-list constraint))

(defgeneric infer (expr type-env)
  (:documentation "returns (`VALUES' TYPED-IR1 TYPE CONSTRAINTS)

where:
TYPED-IR1 is a new node which is like EXPR but with type annotations,
TYPE is the inferred type of EXPR, and
CONSTRAINTS is an (`ASSOCIATION-LIST' `TYPE' `TYPE') denoting the constraints to solve"))

(defmethod infer ((expr variable) type-env)
  (let* ((name (name expr))
         (type (instantiate (type-env-lookup type-env name)))
         (new-node (make-instance 'variable
                                  :type type
                                  :name name)))
    (values new-node type ())))

(defmethod infer ((expr funcall) type-env)
  (multiple-value-bind (func-node func-type func-constraints)
      (infer (func expr) type-env)
    (iter
      (for arg-expr in-vector (args expr))
      (for (values arg-node arg-type arg-constraint) = (infer arg-expr type-env))
      (collect arg-node into arg-nodes result-type (vector expr))
      (collect arg-type into arg-types result-type (vector type))
      (unioning arg-constraint into arg-constraints)
      (finally
       (let* ((return-type (new-type-variable))
              (arrow-type (make-instance 'arrow
                                         :inputs arg-types
                                         :output return-type))
              (funcall-constraints (list (eqv arrow-type func-type)))
              (all-constraints (append funcall-constraints func-constraints arg-constraints))
              (new-node (make-instance 'funcall
                                       :type return-type
                                       :func func-node
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
    (for binding in-vector (bindings expr))
    (for arg-type = (new-type-variable binding))
    (for scheme = (make-constant-scheme arg-type))
    (collect arg-type into arg-types result-type (vector type))
    (push (cons binding scheme) function-env)
    (finally
     (multiple-value-bind (body return-type constraints)
         (infer (body expr) function-env)
      (let* ((arrow-type (make-instance 'arrow
                                       :inputs arg-types
                                       :output return-type))
             (new-node (make-instance 'lambda
                                      :type arrow-type
                                      :bindings (bindings expr)
                                      :body body)))
        (return (values new-node
                        arrow-type
                        constraints)))))))

(|:| #'extend-type-env-for-def (-> (definition type-env) type-env))
(defun extend-type-env-for-def (def type-env)
  (acons (name def) (scheme def)
         type-env))

(|:| #'infer-definition (-> (definition type-env) (values definition constraints type-env &optional)))
(defun infer-definition (untyped type-env)
  "add type information to an untyped `DEFINITION', returning three values:

a new `DEFINITION', a `CONSTRAINTS' object describing the
requirements on that definition, and a `TYPE-ENV' that extends
TYPE-ENV with information about the new definition."
  (multiple-value-bind (new-initform type constraints)
      (infer (initform untyped) type-env)
    (let* ((new-def (make-instance 'definition
                                   :name (name untyped)
                                   :initform new-initform
                                   :scheme (generalize type type-env))))
      (values new-def
              constraints
              (extend-type-env-for-def new-def type-env)))))

(defmethod infer ((expr let) type-env)
  (multiple-value-bind (new-def def-constraints local-env)
      (infer-definition (def expr) type-env)
    (multiple-value-bind (new-body return-type body-constraints)
        (infer (body expr) local-env)
      (values (make-instance 'let
                             :type return-type
                             :def new-def
                             :body new-body)
              return-type
              (append def-constraints body-constraints)))))

(defmethod infer ((expr if) type-env)
  (multiple-value-bind (pred-expr pred-type pred-constraints)
      (infer (predicate expr) type-env)
    (multiple-value-bind (then-expr then-type then-constraints)
        (infer (then-case expr) type-env)
      (multiple-value-bind (else-expr else-type else-constraints)
          (infer (else-case expr) type-env)
        (let* ((new-node (make-instance 'if
                                        :type then-type
                                        :predicate pred-expr
                                        :then-case then-expr
                                        :else-case else-expr))
               (if-constraints (list (eqv pred-type *boolean*)
                                     (eqv then-type else-type)))
               (all-constraints
                 (append if-constraints pred-constraints then-constraints else-constraints)))
          (values new-node
                  then-type
                  all-constraints))))))

(defmethod infer ((expr prog2) type-env)
  (multiple-value-bind (side-effect-expr side-effect-type side-effect-constraints)
      (infer (side-effect expr) type-env)
    (declare (ignore side-effect-type))
    (multiple-value-bind (return-expr return-type return-constraints)
        (infer (return-value expr) type-env)
      (values (make-instance 'prog2
                             :type return-type
                             :side-effect side-effect-expr
                             :return-value return-expr)
              return-type
              (append side-effect-constraints return-constraints)))))

(defmethod infer ((expr quote) type-env)
  (declare (ignorable type-env))
  (let* ((type (etypecase (it expr)
                (boolean *boolean*)
                (fixnum *fixnum*))))
    (values (make-instance 'quote
                           :type type
                           :it (it expr))
            type
            ())))

(define-special *ctor-schemes* (hash-map-of symbol type-scheme))

(|:| #'ctor-schemes-map (-> ((vector type-scheme))
                            (hash-map-of symbol type-scheme)))
(defun ctor-schemes-map (types)
  (iter (with map = (make-hash-table :test #'eq))
    (for scheme in-vector types)
    (for type = (body scheme))
    (etypecase type
      (struct (setf (gethash (name type) map) scheme))
      (enum (iter (for variant in-vector (variants type))
              (setf (gethash (name variant) map) scheme))))
    (finally (return map))))

(|:| #'type-for-ctor (-> (symbol) type))
(defun type-for-ctor (ctor)
  (instantiate (gethash ctor *ctor-schemes*)))

(defmethod infer ((expr discriminant-p) type-env)
  (multiple-value-bind (value-expr value-type value-constraints)
      (infer (value expr) type-env)
    (let* ((enum-type (type-for-ctor (variant expr))))
      (values (shallow-copy expr
                            :value value-expr
                            :type *boolean*)
              *boolean*
              (cons (eqv value-type enum-type) value-constraints)))))

(defmethod infer ((expr assert-variant) type-env)
  (multiple-value-bind (src src-type src-constraints)
      (infer (src expr) type-env)
    (let* ((agg-type (type-for-ctor (constructor expr))))
      (values (shallow-copy expr
                            :src src
                            :type agg-type)
              agg-type
              (cons (eqv agg-type src-type) src-constraints)))))

(defmethod infer ((expr match-exhausted) type-env)
  (declare (ignorable type-env))
  (let* ((ty (new-type-variable "bottom-type-")))
    (values (shallow-copy expr :type ty)
            ty
            ())))

(defgeneric find-variant (agg ctor)
  (:method ((agg struct) (ctor symbol))
    (assert (eq (name agg) ctor))
    agg)
  (:method ((agg enum) (ctor symbol))
    (or (find ctor (variants agg) :key #'name :test #'eq)
        (error "Variant ~a not in enum ~a" ctor agg))))

(defmethod infer ((expr field-value) type-env)
  (multiple-value-bind (agg-expr agg-ty agg-constraints)
      (infer (aggregate expr) type-env)
    (let* ((ctor (constructor expr))
           (ctor-ty (type-for-ctor ctor))
           (variant (find-variant ctor-ty ctor))
           (field-ty (aref (elts variant) (idx expr))))
      (values (shallow-copy expr
                            :type field-ty
                            :aggregate agg-expr)
              field-ty
              (cons (eqv ctor-ty agg-ty)
                    agg-constraints)))))

(defmethod infer ((expr and) type-env)
  (multiple-value-bind (lhs-expr lhs-ty lhs-constraints)
      (infer (lhs expr) type-env)
    (multiple-value-bind (rhs-expr rhs-ty rhs-constraints)
        (infer (rhs expr) type-env)
      (values (shallow-copy expr
                            :type lhs-ty
                            :lhs lhs-expr
                            :rhs rhs-expr)
              lhs-ty
              (append (list (eqv lhs-ty rhs-ty))
                      lhs-constraints
                      rhs-constraints)))))

(|:| #'infer-program (-> (program) (values program constraints &optional)))
(defun infer-program (program &aux (*ctor-schemes* (ctor-schemes-map (types program))))
  (iter
    (with type-env = ())
    (for def in-vector (definitions program))
    (for (values new-def def-constraints new-env) = (infer-definition def type-env))
    (setf type-env new-env)
    (appending def-constraints into constraints)
    (collect new-def into definitions result-type (vector definition))
    (finally
     (multiple-value-bind (entry result-type entry-constraints)
         (infer (entry program) type-env)
       (declare (ignore result-type))
       (return
         (values
          (shallow-copy program
                        :definitions definitions
                        :entry entry)
          (append entry-constraints constraints)))))))

