(uiop:define-package :hindley-milner/monomorphize
    (:nicknames :monomorphize)
  (:mix
   :hindley-milner/prologue
   :hindley-milner/ir1
   :iterate
   :trivial-types
   :cl)
  (:import-from :genhash
   :make-generic-hash-table :hashref :register-test-designator)
  (:import-from :hindley-milner/typecheck/unify
   :unify)
  (:import-from
   :alexandria
   :with-gensyms
   :make-gensym)
  (:import-from :hindley-milner/typecheck/substitute
   :apply-substitution)
  (:export
   :monomorphize-program))
(cl:in-package :hindley-milner/monomorphize)

(defgeneric hash-type (type))

(defmethod hash-type ((vec vector))
  (iter
    (with hash = 0)
    (for el in-vector vec)
    (setf hash (logxor hash
                       (hash-type el)))
    (finally (return hash))))

(defmethod hash-type ((obj arrow))
  (logxor (hash-type (arrow-inputs obj))
          (hash-type (arrow-output obj))))

(defmethod hash-type ((obj type-primitive))
  (sxhash (type-primitive-name obj)))

(defmethod hash-type ((obj type-variable))
  (sxhash (type-variable-name obj)))

(defgeneric type-equalp (lhs rhs))

(defmethod type-equalp ((lhs vector) (rhs vector))
  (iter
    (for lhs-el in-vector lhs)
    (for rhs-el in-vector rhs)
    (unless (type-equalp lhs-el rhs-el)
      (return nil))
    (finally (return t))))

(defmethod type-equalp ((lhs arrow) (rhs arrow))
  (and (type-equalp (arrow-inputs lhs) (arrow-inputs rhs))
       (type-equalp (arrow-output lhs) (arrow-output rhs))))

(defmethod type-equalp ((lhs type-primitive) (rhs type-primitive))
  (eq (type-primitive-name lhs) (type-primitive-name rhs)))

(defmethod type-equalp ((lhs type-variable) (rhs type-variable))
  (eq (type-variable-name lhs) (type-variable-name rhs)))

(defmethod type-equalp (lhs rhs)
  (equalp lhs rhs))

(register-test-designator 'type-equalp #'hash-type #'type-equalp)

(define-class lexenv
  ((polymorphic-values (hash-map-of symbol expr)
                       :initform (make-generic-hash-table :test 'eq))
   (monomorphic-values (association-list symbol expr)
                       :initform nil)
   (existing-monomorphizations (hash-map-of symbol (hash-map-of type symbol))
                               :initform (make-generic-hash-table :test 'eq))
   (parent (or lexenv null)
           :initform nil)))

(|:| #'push-poly-obj (-> (lexenv definition) void))
(defun push-poly-obj (lexenv def)
  (setf (hashref (definition-name def)
                 (lexenv-polymorphic-values lexenv))
        (definition-initform def))
  (values))

(|:| #'collect-polymorphic-values (-> (expr lexenv) expr))
(defun collect-polymorphic-values (let lexenv)
  "returns the body of LET, mutating LEXENV along the way."
  (iter    
    (with top-level-expr = let)
    (unless (typep top-level-expr 'let)
      (return top-level-expr))
    (push-poly-obj lexenv (let-def top-level-expr))
    (setf top-level-expr (let-body top-level-expr))))

(|:| #'existing-monomorphization-second-level-map (-> (lexenv symbol) (hash-map-of type symbol)))
(defun existing-monomorphization-second-level-map (lexenv poly-name)
  (ensure-get poly-name
              (lexenv-existing-monomorphizations lexenv)
              (make-generic-hash-table :test 'type-equalp)))

(|:| #'find-existing-monomorphization (-> (lexenv symbol type) (optional symbol)))
(defun find-existing-monomorphization (lexenv poly-name mono-type)
  (multiple-value-bind (val present-p)
      (hashref mono-type
               (existing-monomorphization-second-level-map lexenv poly-name))
    (cond (present-p val)
          ((lexenv-parent lexenv) (find-existing-monomorphization (lexenv-parent lexenv)
                                                                  poly-name
                                                                  mono-type))
          (:otherwise nil))))

(|:| #'insert-into-existing-monomorphizations-map (-> (lexenv symbol type symbol) void))
(defun insert-into-existing-monomorphizations-map (lexenv poly-name mono-type mono-name)
  (setf (hashref mono-type
                 (existing-monomorphization-second-level-map lexenv poly-name))
        mono-name)
  (values))

(|:| #'monomorphize-poly-expr (-> (lexenv expr symbol type) symbol))
(defun monomorphize-poly-expr (lexenv poly-expr poly-name mono-type)
  (let* ((poly-type (expr-type poly-expr))
         (substitution (unify mono-type poly-type))
         (mono-expr (monomorphize (apply-substitution substitution poly-expr) lexenv))
         (mono-name (make-gensym poly-name)))
    (push (cons mono-name mono-expr) (lexenv-monomorphic-values lexenv))
    (insert-into-existing-monomorphizations-map lexenv poly-name mono-type mono-name)
    mono-name))

(|:| #'add-new-monomorphization (-> (lexenv symbol type) symbol))
(defun add-new-monomorphization (lexenv poly-name mono-type)
  (multiple-value-bind (poly-expr present-p)
      (hashref poly-name (lexenv-polymorphic-values lexenv))
    (cond (present-p (monomorphize-poly-expr lexenv
                                             poly-expr
                                             poly-name
                                             mono-type))
          ((lexenv-parent lexenv) (add-new-monomorphization (lexenv-parent lexenv)
                                                            poly-name
                                                            mono-type))
          (:otherwise (error "unbound poly-name ~a" poly-name)))))

(|:| #'monomorphize-symbol (-> (symbol type lexenv) symbol))
(defun monomorphize-symbol (symbol target-type lexenv)
  (or (find-existing-monomorphization lexenv symbol target-type)
      (add-new-monomorphization lexenv symbol target-type)))

(defgeneric monomorphize (expr lexenv)
  (:documentation "returns a new `IR1:EXPR' that is like EXPR except references to polymorphic values are replaced with monomorphic versions."))

(defmethod monomorphize ((var variable) lexenv)
  (make-instance 'variable
                 :type (expr-type var)
                 :name (monomorphize-symbol (variable-name var)
                                            (expr-type var)
                                            lexenv)))

(defmethod monomorphize ((quote quote) lexenv)
  (declare (ignorable lexenv))
  quote)

(defmacro define-monomorphize (class &body body)
  "define a method on `MONOMORPHIZE' for CLASS.

CLASS should be a symbol which names a class.

within BODY, the symbol CLASS is bound to the instance being
monomorphized, and `RECURSE' is bound to a function of one argument
which calls `MONOMORPHIZE' on its argument with the same `LEXENV'.

for example:

  (DEFINE-MONOMORPHIZE FUNCALL
    (MAKE-FUNCALL (FUNCALL-TYPE FUNCALL)
                  (RECURSE (FUNCALL-FUNCTION FUNCALL))
                  (RECURSE (FUNCALL-ARG FUNCALL))))

defines a method for the class `FUNCALL' which recurses on its slots
`FUNCTION' and `ARG', but passes its slot `TYPE' unchanged."
  (multiple-value-bind (class lexenv) (etypecase class
                                        (symbol (values class (make-gensym 'lexenv)))
                                        (list (values (first class) (second class))))
    `(defmethod monomorphize ((,class ,class) ,lexenv)
       ,(format nil "`MONOMOPHIZE' method for ~s defined by `DEFINE-MONOMORPHIZE'" class)
       (flet ((recurse (thing &optional (,lexenv ,lexenv))
                (monomorphize thing ,lexenv)))
         ,@body))))

(define-monomorphize vector
  (iter
    (for expr in-vector vector)
    (collect (recurse expr) result-type (vector expr))))

(define-monomorphize funcall
  (let* ((function (recurse (funcall-function funcall)))
         (return-type (arrow-output (expr-type function)))
         (args (recurse (funcall-args funcall))))
    (make-instance 'funcall
                   :type return-type
                   :function (recurse (funcall-function funcall))
                   :args args)))

(define-monomorphize (lambda enclosing-env)
  (iter
    (with local-env = (make-instance 'lexenv
                                     :parent enclosing-env))
    (for bound-name in-vector (lambda-bindings lambda))
    (for bound-type in-vector (arrow-inputs (expr-type lambda)))
    (insert-into-existing-monomorphizations-map local-env
                                                bound-name
                                                bound-type
                                                bound-name)
    (finally
     (let* ((body (recurse (lambda-body lambda) local-env))
            (return-type (expr-type body)))
       (return
         (make-instance 'lambda
                        :type (make-instance 'arrow
                                             :inputs (arrow-inputs (expr-type lambda))
                                             :output return-type)
                        :bindings (lambda-bindings lambda)
                        :body body))))))

(|:| #'make-monomorphic-def (-> (symbol expr) monomorphic))
(defun make-monomorphic-def (binding initform)
  (make-instance 'monomorphic
                 :name binding
                 :type (expr-type initform)
                 :initform initform))

(define-monomorphize (let enclosing-env)
  (let* ((local-env (make-instance 'lexenv
                                   :parent enclosing-env))
         (poly-body (collect-polymorphic-values let local-env))
         (mono-body (recurse poly-body local-env)))
    (iter
      (with body = mono-body)
      (for (binding . initform) in (lexenv-monomorphic-values local-env))
      (for def = (make-monomorphic-def binding initform))
      (setf body (make-instance 'let
                                :def def
                                :type (expr-type body)
                                :body body))
      (finally (return body)))))

(define-monomorphize if
  (let* ((predicate (recurse (if-predicate if)))
         (then-case (recurse (if-then-case if)))
         (else-case (recurse (if-else-case if)))
         (return-type (expr-type then-case)))
    (make-instance 'if
                   :type return-type
                   :predicate predicate
                   :then-case then-case
                   :else-case else-case)))

(define-monomorphize primop
  (make-instance 'primop
                 :type (expr-type primop)
                 :op (primop-op primop)
                 :args (recurse (primop-args primop))))

(define-monomorphize prog2
  (let* ((side-effect (recurse (prog2-side-effect prog2)))
         (return-value (recurse (prog2-return-value prog2)))
         (return-type (expr-type return-value)))
    (make-instance 'prog2
                   :type return-type
                   :side-effect side-effect
                   :return-value return-value)))

(|:| #'monomorphize-program (-> (program) program))
(defun monomorphize-program (program)
  "returns a `IR1:EXPR' that is like `PROGRAM', except all polymorphic values are replacecd with equivalent monomorphic values"
  (iter
    (with lexenv = (make-instance 'lexenv))
    (for def in-vector (program-definitions program))
    (push-poly-obj lexenv def)
    (finally
     (iter
       (with monomorphic-entry = (monomorphize (program-entry program) lexenv))
       (with monomorphic-defs = (adjustable-vector definition))
       (for (binding . initform) in (lexenv-monomorphic-values lexenv))
       (vector-push-extend (make-monomorphic-def binding initform)
                           monomorphic-defs)
       (finally (return-from monomorphize-program
                  (make-instance 'program
                                 :definitions monomorphic-defs
                                 :entry monomorphic-entry)))))))
