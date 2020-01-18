(uiop:define-package :hindley-milner/monomorphize
    (:nicknames :monomorphize)
    (:mix
     :hindley-milner/typecheck/type
     :hindley-milner/typecheck/typed-ir1
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
  (logand (+ (hash (->-input obj))
             (hash (->-output obj)))
          most-positive-fixnum))

(defmethod equalp ((lhs type-primitive) (rhs type-primitive))
  (eq (type-primitive-name lhs) (type-primitive-name rhs)))
(defmethod hash ((obj type-primitive))
  (hash (type-primitive-name obj)))

(defmethod equalp ((lhs type-variable) (rhs type-variable))
  (eq (type-variable-name lhs) (type-variable-name rhs)))
(defmethod hash ((obj type-variable))
  (hash (type-variable-name obj)))

(gefjon-utils:defclass lexenv
    ((polymorphic-values (hash-map-of symbol expr))
     (monomorphic-values (hash-map-of symbol expr))
     (existing-monomorphizations (hash-map-of symbol (hash-map-of type symbol)))
     (parent (or lexenv null)))
  :superclasses (gefjon-utils:print-all-slots-mixin))

(defun make-empty-lexenv ()
  (make-instance 'lexenv
                 :polymorphic-values (make-hash-map :test #'eq)
                 :monomorphic-values (make-hash-map :test #'eq)
                 :existing-monomorphizations (make-hash-map :test #'eq)
                 :parent nil))

(defun push-poly-obj (lexenv let)
  (setf (get (let-binding let)
                 (lexenv-polymorphic-values lexenv))
        (let-initform let))
  (values))

(defun collect-polymorphic-values (program)
  "returns (`VALUES' LEXENV ENTRY)

where LEXENV is a `LEXENV' object whose `POLYMORPHIC' records have been populated, and
ENTRY is an `EXPR' other than a `LET' where PROGRAM will begin execution"
  (iter
    (with lexenv = (make-empty-lexenv))
    (with top-level-expr = program)
    (unless (typep top-level-expr 'let)
      (return (values lexenv top-level-expr)))
    (push-poly-obj lexenv top-level-expr)
    (setf top-level-expr (let-body top-level-expr))))

(defun find-poly-obj-or-error (lexenv poly-name)
  (multiple-value-bind (value present-p) (get poly-name (lexenv-polymorphic-values lexenv))
    (cl:if present-p value
           (error "polymorphic value ~s not found" poly-name))))

(defun existing-monomorphization-second-level-map (lexenv poly-name)
  (multiple-value-bind (map foundp) (get poly-name (lexenv-existing-monomorphizations lexenv))
    (unless foundp
      (setf map (make-hash-map :test #'equalp))
      (setf (get poly-name (lexenv-existing-monomorphizations lexenv))
            map))
    map))

(defun find-existing-monomorphization (lexenv poly-name mono-type)
  (get mono-type (existing-monomorphization-second-level-map lexenv poly-name)))

(defun insert-into-existing-monomorphizations-map (lexenv poly-name mono-type mono-name)
  (setf (get mono-type (existing-monomorphization-second-level-map lexenv poly-name))
        mono-name)
  (values))

(defun add-new-monomorphization (lexenv poly-name mono-type)
  (let* ((poly-expr (find-poly-obj-or-error lexenv poly-name))
         (poly-type (expr-type poly-expr))
         (substitution (unify mono-type poly-type))
         (mono-expr (apply-substitution substitution poly-expr))
         (mono-name (make-gensym poly-name)))
    (setf (get mono-name (lexenv-monomorphic-values lexenv))
          mono-expr)
    (insert-into-existing-monomorphizations-map lexenv poly-name mono-type mono-name)
    mono-name))

(defgeneric monomorphize (expr lexenv)
  (:documentation "returns a new `TYPED-IR1:EXPR' that is like EXPR except references to polymorphic values are replaced with monomorphic versions."))

(defmethod monomorphize ((var variable) lexenv)
  (make-instance 'variable
                 :type (expr-type var)
                 :name (or (print (find-existing-monomorphization lexenv
                                                                  (variable-name var)
                                                                  (expr-type var)))
                           (print (add-new-monomorphization lexenv
                                                            (variable-name var)
                                                            (expr-type var))))))

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
(define-monomorphize let
  (make-instance 'let
                 :type (expr-type let)
                 :binding (let-binding let)
                 :scheme (let-scheme let)
                 :initform (recurse (let-initform let))
                 :body (recurse (let-body let))))

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

(defun monomorphize-program (program)
  "returns (`VALUES' ENTRY LEXENV)

where ENTRY is a `TYPED-IR1:EXPR' and
LEXENV is a `LEXENV' with all its fields populated"
  (multiple-value-bind (lexenv entry) (collect-polymorphic-values program)
    (cl:let ((monomorphic (monomorphize entry lexenv)))
      (values monomorphic lexenv))))
