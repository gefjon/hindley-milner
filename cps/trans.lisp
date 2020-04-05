(uiop:define-package :hindley-milner/cps/trans
    (:mix
     :hindley-milner/cps/expr
     :hindley-milner/prologue
     :iterate
     :trivial-types
     :cl)
  (:import-from :alexandria
   :make-gensym)
  (:import-from :gefjon-utils
   :adjustable-vector :make-adjustable-vector :specialized-vector :|:| :->)
  (:import-from :hindley-milner/ir1)
  (:import-from :hindley-milner/monomorphize)
  (:export :cps-transform :*exit-continuation*))
(cl:in-package :hindley-milner/cps/trans)

(|:| #'make-continuation-arg (-> (&optional symbol) variable))
(defun make-continuation-arg (&optional (name 'cont))
  (make-instance 'variable
                 :name (make-gensym name)
                 :type :continuation))

(deftype intermediate-terms ()
  '(association-list variable ir1:expr))

(deftype lexenv ()
  '(proper-list variable))

(|:| #'find-variable (-> (symbol lexenv) variable))
(defun find-variable (name lexenv)
  (or (find name lexenv :test #'eq :key #'variable-name)
      (error "undefined variable ~s" name)))

(|:| #'compute-intermediate-terms (-> (intermediate-terms expr lexenv) expr))
(defun compute-intermediate-terms (intermediate-terms body lexenv)
  (iter
    (with expr = body)
    (for (var . initform) in intermediate-terms)
    (setf expr (transform-binding initform var expr :lexenv lexenv))
    (finally (return expr))))

(defgeneric transform-type (type)
  (:documentation "transform an IR1:TYPE into a CPS:TYPE"))

(defgeneric transform-to-expr (ir1-expr
                               &key
                                 current-continuation
                                 lexenv
                               &allow-other-keys))

(defgeneric transform-to-var (ir1-expr
                              &key
                                lexenv
                              &allow-other-keys)
  (:documentation "returns (VALUES VARIABLE INTERMEDIATE-TERMS)"))

(defgeneric transform-binding (ir1-initform var body
                               &key
                                 lexenv
                               &allow-other-keys)
  (:documentation "construct a `CPS:EXPR' which binds the `VARIABLE' VAR to IR1-INITFORM within the `CPS:EXPR' BODY.

returns as a secondary value an (ASSOCIATION-LIST VARIABLE IR1:EXPR)
of intermediate terms which must be constructed around the primary
value.

LEXENV should be an (ASSOCIATION-LIST SYMBOL VARIABLE) which is used
to preserve EQ-ness of variables"))

;;; transform-type methods

(defmethod transform-type ((type ir1:type-primitive))
  (ecase (ir1:type-primitive-name type)
        (boolean :boolean)
        (fixnum :fixnum)
        (null :void)))

(defmethod transform-type ((type ir1:->))
  (declare (ignorable type))
  :function)

;;; transform-to-expr methods

(defmethod transform-to-expr ((expr ir1:variable)
                              &key
                                current-continuation
                                lexenv
                              &allow-other-keys)
  (make-instance 'throw
                 :cont current-continuation
                 :arg (find-variable (ir1:variable-name expr) lexenv)))

(|:| #'funcall-vars-and-intermediate-terms
     (-> (ir1:funcall lexenv)
         (values (vector variable) intermediate-terms &optional)))
(defun arg-vec-vars-and-terms (arg-vec lexenv)
  "returns (VALUES ARG-VARS INTERMEDIATE-TERMS)

where ARG-VARS is a (VECTOR VARIABLE) suitable for a `CPS:FUNCALL',
and INTERMEDIATE-TERMS is an (ASSOCIATION-LIST VARIABLE IR1:EXPR) of
terms that must be computed prior to the call."
  (iter
    (for arg in-vector arg-vec)
    (for (values arg-var compute-arg) = (transform-to-var arg :lexenv lexenv))
    (collect arg-var into arg-vars result-type (vector variable))
    (appending compute-arg into intermediate-terms)
    (finally (return (values arg-vars intermediate-terms)))))

(defmethod transform-to-expr ((expr ir1:funcall)
                              &key
                                current-continuation
                                lexenv
                              &allow-other-keys)
  (multiple-value-bind (arg-vars arg-terms)
      (arg-vec-vars-and-terms (ir1:funcall-args expr) lexenv)
    (multiple-value-bind (func-var func-terms)
        (transform-to-var (ir1:funcall-function expr) :lexenv lexenv)
      (let* ((apply-expr (make-instance 'apply
                                        :func func-var
                                        :args arg-vars
                                        :continuation current-continuation))
             (compute-args (compute-intermediate-terms arg-terms
                                                       apply-expr
                                                       lexenv))
             (compute-func (compute-intermediate-terms func-terms
                                                       compute-args
                                                       lexenv)))
        compute-func))))


(defmethod transform-to-expr ((expr monomorphize:mono-let)
                              &key
                                current-continuation
                                lexenv
                              &allow-other-keys)
  (let* ((var-name (monomorphize:mono-let-binding expr))
         (var-type (transform-type (monomorphize:mono-let-bound-type expr)))
         (var (make-instance 'variable
                             :name var-name
                             :type var-type))
         (bound-env (cons var lexenv))
         (body (transform-to-expr (monomorphize:mono-let-body expr)
                                  :current-continuation current-continuation
                                  :lexenv bound-env))
         (binding-form (transform-binding (monomorphize:mono-let-initform expr)
                                          var
                                          body
                                          :lexenv lexenv)))
    binding-form))

(defmethod transform-to-expr ((expr ir1:if)
                              &key
                                current-continuation
                                lexenv
                              &allow-other-keys)
  (multiple-value-bind (pred-var compute-pred)
      (transform-to-var (ir1:if-predicate expr) :lexenv lexenv)
    (let* ((then-expr
             (transform-to-expr (ir1:if-then-case expr)
                                :current-continuation current-continuation
                                :lexenv lexenv))
           (else-expr
             (transform-to-expr (ir1:if-else-case expr)
                                :current-continuation current-continuation
                                :lexenv lexenv))
           (if-expr
             (make-instance 'if
                            :predicate pred-var
                            :then-clause then-expr
                            :else-clause else-expr)))
      (compute-intermediate-terms compute-pred if-expr lexenv))))

(defmethod transform-to-expr ((expr ir1:prog2)
                              &key
                                current-continuation
                                lexenv
                              &allow-other-keys)
  (let* ((ret-expr (transform-to-expr (ir1:prog2-return-value expr)
                                      :current-continuation current-continuation
                                      :lexenv lexenv))
         (ignore-cont-name (make-continuation-arg 'prog2))
         (side-effect-expr (transform-to-expr (ir1:prog2-side-effect expr)
                                              :current-continuation ignore-cont-name
                                              :lexenv lexenv))
         (ignore-var (make-instance 'variable
                                    :name (make-gensym 'ignore)
                                    :type (transform-type
                                           (ir1:expr-type
                                            (ir1:prog2-side-effect expr)))))
         (ignore-cont (make-instance 'cont
                                     :name ignore-cont-name
                                     :arg ignore-var
                                     :body ret-expr
                                     :in side-effect-expr)))
    ignore-cont))

(defmethod transform-to-expr ((expr ir1:expr)
                              &key
                                current-continuation
                                lexenv
                              &allow-other-keys)
  (let* ((var (make-instance 'variable
                             :name (make-gensym 'tmp)
                             :type (transform-type (ir1:expr-type expr))))
         (body (make-instance 'throw
                              :arg var
                              :cont current-continuation)))
    (transform-binding expr var body :lexenv lexenv)))

;;; transform-to-var methods

(defmethod transform-to-var ((expr ir1:variable)
                             &key
                               lexenv
                             &allow-other-keys)
  (find-variable (ir1:variable-name expr) lexenv))

(defmethod transform-to-var ((expr ir1:expr)
                             &key
                             &allow-other-keys)
  (let* ((variable (make-instance 'variable
                                  :name (make-gensym 'var)
                                  :type (transform-type (ir1:expr-type expr)))))
    (values variable (acons variable expr ()))))

;;; transform-binding methods

(|:| #'lambda-arg-vars (-> (ir1:lambda) (vector variable)))
(defun lambda-arg-vars (ir1-lambda)
  (iter
    (for arg-name in-vector (ir1:lambda-bindings ir1-lambda))
    (for arg-type in-vector (ir1:->-inputs (ir1:expr-type ir1-lambda)))
    (collect (make-instance 'variable
                            :name arg-name
                            :type (transform-type arg-type))
      result-type (vector variable))))

(|:| #'augment-lexenv-for-func (-> (lexenv (vector variable)) lexenv))
(defun augment-lexenv-for-func (enclosing-env arglist)
  (iter
    (with env = enclosing-env)
    (for arg in-vector arglist)
    (setf env (cons arg env))
    (finally (return env))))

(defmethod transform-binding ((initform ir1:lambda) var body
                              &key
                                lexenv
                              &allow-other-keys)
  (let* ((continuation-arg (make-continuation-arg 'return))
         (args (lambda-arg-vars initform))
         (fenv (augment-lexenv-for-func lexenv args))
         (fbody (transform-to-expr (ir1:lambda-body initform)
                                   :current-continuation continuation-arg
                                   :lexenv fenv)))
    (make-instance 'func
                   :name var
                   :arglist args
                   :continuation-arg continuation-arg
                   :body fbody
                   :in body)))

(defmethod transform-binding ((initform ir1:quote) var body
                              &key &allow-other-keys)
  (make-instance 'const
                 :name var
                 :value (ir1:quote-it initform)
                 :in body))

(defmethod transform-binding ((initform ir1:primop) var body
                              &key lexenv &allow-other-keys)
  (multiple-value-bind (arg-vars arg-terms)
      (arg-vec-vars-and-terms (ir1:primop-args initform) lexenv)
    (let* ((let-expr (make-instance 'let
                                    :var var
                                    :prim-op (ir1:primop-op initform)
                                    :args arg-vars
                                    :in body)))
      (compute-intermediate-terms arg-terms
                                  let-expr
                                  lexenv))))

(defmethod transform-binding ((initform ir1:expr) var body
                              &key
                                lexenv
                              &allow-other-keys)
  (let* ((cont-name (make-continuation-arg))
         (expr (transform-to-expr initform
                                  :lexenv lexenv
                                  :current-continuation cont-name))
         (cont-expr (make-instance 'cont
                                   :name cont-name
                                   :arg var
                                   :body body
                                   :in expr)))
    cont-expr))

(defvar *exit-continuation* (make-instance 'variable
                                           :name 'exit
                                           :type :continuation)
  "the continuation to exit the program")

(defun cps-transform (typed-ir1-program)
  (transform-to-expr typed-ir1-program
                     :current-continuation *exit-continuation*))
