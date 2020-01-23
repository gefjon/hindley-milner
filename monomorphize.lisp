(uiop:define-package :hindley-milner/monomorphize
    (:nicknames :monomorphize)
    (:mix
     :hindley-milner/typecheck/type
     :hindley-milner/ir1
     :iterate
     :trivial-types
     :cl)
  (:shadowing-import-from :generic-cl
   :equalp :hash :get :hash-map :make-hash-map)
  (:import-from :hindley-milner/typecheck/unify
   :unify)
  (:import-from
   :alexandria
   :with-gensyms
   :make-gensym)
  (:import-from :hindley-milner/typecheck/substitute
   :apply-substitution)
  (:export :monomorphize-program))
(cl:in-package :hindley-milner/monomorphize)

(deftype hash-map-of (&optional key value)
  (declare (ignore key value))
  'hash-map)

(defmethod equalp ((lhs ->) (rhs ->))
  (and (equalp (->-input lhs) (->-input rhs))
       (equalp (->-output lhs) (->-output rhs))))
(defmethod hash ((obj ->))
  (logxor (hash (->-input obj))
          (hash (->-output obj))))

(defmethod equalp ((lhs type-primitive) (rhs type-primitive))
  (eq (type-primitive-name lhs) (type-primitive-name rhs)))
(defmethod hash ((obj type-primitive))
  (hash (type-primitive-name obj)))

(defmethod equalp ((lhs type-variable) (rhs type-variable))
  (eq (type-variable-name lhs) (type-variable-name rhs)))
(defmethod hash ((obj type-variable))
  (hash (type-variable-name obj)))

(gefjon-utils:defclass lexenv
  ((polymorphic-values (hash-map-of symbol expr)
                       :initform (make-hash-map :test #'eq))
   (monomorphic-values (association-list symbol expr)
                       :initform nil)
   (existing-monomorphizations (hash-map-of symbol (hash-map-of type symbol))
                               :initform (make-hash-map :test #'eq))
   (parent (or lexenv null)
           :initform nil))
  :superclasses (gefjon-utils:print-all-slots-mixin))

(defun push-poly-obj (lexenv let)
  (setf (get (let-binding let)
                 (lexenv-polymorphic-values lexenv))
        (let-initform let))
  (values))

(defun collect-polymorphic-values (program lexenv)
  "returns the body of PROGRAM, mutating LEXENV along the way."
  (iter    
    (with top-level-expr = program)
    (unless (typep top-level-expr 'let)
      (return top-level-expr))
    (push-poly-obj lexenv top-level-expr)
    (setf top-level-expr (let-body top-level-expr))))

(defun existing-monomorphization-second-level-map (lexenv poly-name)
  (multiple-value-bind (map foundp) (get poly-name (lexenv-existing-monomorphizations lexenv))
    (unless foundp
      (setf map (make-hash-map :test #'equalp))
      (setf (get poly-name (lexenv-existing-monomorphizations lexenv))
            map))
    map))

(defun find-existing-monomorphization (lexenv poly-name mono-type)
  (multiple-value-bind (val present-p) (get mono-type (existing-monomorphization-second-level-map lexenv poly-name))
    (cond (present-p val)
          ((lexenv-parent lexenv) (find-existing-monomorphization (lexenv-parent lexenv)
                                                                  poly-name
                                                                  mono-type))
          (:otherwise nil))))

(defun insert-into-existing-monomorphizations-map (lexenv poly-name mono-type mono-name)
  (setf (get mono-type (existing-monomorphization-second-level-map lexenv poly-name))
        mono-name)
  (values))

(defun monomorphize-poly-expr (lexenv poly-expr poly-name mono-type)
  (let* ((poly-type (expr-type poly-expr))
         (substitution (unify mono-type poly-type))
         (mono-expr (apply-substitution substitution poly-expr))
         (mono-name (make-gensym poly-name)))
    (push (cons mono-name mono-expr) (lexenv-monomorphic-values lexenv))
    (insert-into-existing-monomorphizations-map lexenv poly-name mono-type mono-name)
    mono-name))

(defun add-new-monomorphization (lexenv poly-name mono-type)
  (multiple-value-bind (poly-expr present-p) (get poly-name (lexenv-polymorphic-values lexenv))
    (cond (present-p (monomorphize-poly-expr lexenv
                                             poly-expr
                                             poly-name
                                             mono-type))
          ((lexenv-parent lexenv) (add-new-monomorphization (lexenv-parent lexenv)
                                                            poly-name
                                                            mono-type))
          (:otherwise (error "unbound poly-name ~a" poly-name)))))

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
  (with-gensyms (thing lexenv)
    `(defmethod monomorphize ((,class ,class) ,lexenv)
       ,(format nil "`MONOMOPHIZE' method for ~s defined by `DEFINE-MONOMORPHIZE'" class)
       (flet ((recurse (,thing)
                (monomorphize ,thing ,lexenv)))
         ,@body))))

(define-monomorphize funcall
  (make-instance 'funcall
                 :type (expr-type funcall)
                 :function (recurse (funcall-function funcall))
                 :arg (recurse (funcall-arg funcall))))

(define-monomorphize lambda
  (make-instance 'lambda
                 :type (expr-type lambda)
                 :function (lambda-binding lambda)
                 :arg (recurse (lambda-body lambda))))

;; todo: properly handle polymorphic non-top-level lets
(defmethod monomorphize ((let let) enclosing-env)
  (let* ((local-env (make-instance 'lexenv
                                   :parent enclosing-env))
         (poly-body (collect-polymorphic-values let local-env))
         (mono-body (monomorphize poly-body local-env)))
    (iter
      (with body = mono-body)
      (for (binding . initform) in (lexenv-monomorphic-values local-env))
      (setf body (make-instance 'let
                                :type (expr-type body)
                                :binding binding
                                :scheme (expr-type initform)
                                :initform initform
                                :body body))
      (finally (return body)))))

(define-monomorphize if
  (make-instance 'if
                 :type (expr-type if)
                 :predicate (recurse (if-predicate if))
                 :then-case (recurse (if-then-case if))
                 :else-case (recurse (if-else-case if))))

(define-monomorphize binop
  (make-instance 'binop
                 :type (expr-type binop)
                 :op (binop-op binop)
                 :lhs (recurse (binop-lhs binop))
                 :rhs (recurse (binop-rhs binop))))

(define-monomorphize prog2
  (make-instance 'prog2
                 :type (expr-type prog2)
                 :side-effect (recurse (prog2-side-effect prog2))
                 :return-value (recurse (prog2-return-value prog2))))

(declaim (ftype (function (expr) expr)
                monomorphize-program))
(defun monomorphize-program (program)
  "returns a `IR1:EXPR' that is like `PROGRAM', except all polymorphic values are replacecd with equivalent monomorphic values"
  (monomorphize program (make-instance 'lexenv)))
