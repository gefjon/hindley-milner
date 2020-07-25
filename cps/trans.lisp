(uiop:define-package :hindley-milner/cps/trans
    (:mix
     :hindley-milner/cps/expr
     :hindley-milner/cps/type
     :hindley-milner/prologue
     :iterate
     :cl)
    (:import-from :trivial-types
     :proper-list :association-list)
  (:import-from :alexandria
   :make-gensym)
  (:import-from :hindley-milner/ir1)
  (:export :cps-transform :*exit-continuation*))
(cl:in-package :hindley-milner/cps/trans)

(|:| #'make-continuation-arg (-> (symbol repr-type) local))
(defun make-continuation-arg (name arg-type)
  (make-instance 'local
                 :name (make-gensym name)
                 :type (make-instance 'function
                                      :inputs (specialized-vector repr-type arg-type))))

(deftype intermediate-terms ()
  '(association-list variable ir1:expr))

(deftype lexenv ()
  '(proper-list variable))

(|:| #'find-variable (-> (symbol lexenv) variable))
(defun find-variable (name lexenv)
  (or (find name lexenv :test #'eq :key #'name)
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
value."))

;;; transform-type methods

(defmethod transform-type ((type ir1:type-primitive))
  (ecase (ir1:name type)
    (:void *void*)
    (:fixnum *fixnum*)
    (:boolean *boolean*)))

(defmethod transform-type ((type ir1:arrow))
  (with-slot-accessors (ir1:inputs ir1:output) type
    (let* ((cont-arg (transform-type ir1:output))
           (cont-fn (make-instance 'function
                                   :inputs (specialized-vector repr-type cont-arg)))
           (normal-args (map '(vector repr-type) #'transform-type
                             (ir1:inputs type)))
           (args (concatenate '(vector repr-type)
                              (list cont-fn)
                              normal-args)))
      (make-instance 'function
                     :inputs args))))

;;; transform-to-expr methods

(defmethod transform-to-expr ((expr ir1:variable)
                              &key
                                current-continuation
                                lexenv
                              &allow-other-keys)
  (make-instance 'apply
                 :func current-continuation
                 :args (specialized-vector variable
                                           (find-variable (ir1:name expr) lexenv))))

(defmethod transform-to-expr ((expr ir1:quote)
                              &key current-continuation
                              &allow-other-keys)
  (make-instance 'apply
                 :func current-continuation
                 :args (specialized-vector variable (transform-to-var expr))))

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
      (arg-vec-vars-and-terms (ir1:args expr) lexenv)
    (multiple-value-bind (func-var func-terms)
        (transform-to-var (ir1:func expr) :lexenv lexenv)
      (let* ((full-arglist (concatenate '(vector variable)
                                        (list current-continuation)
                                        arg-vars))
             (apply-expr (make-instance 'apply
                                        :func func-var
                                        :args full-arglist))
             (compute-args (compute-intermediate-terms arg-terms
                                                       apply-expr
                                                       lexenv))
             (compute-func (compute-intermediate-terms func-terms
                                                       compute-args
                                                       lexenv)))
        compute-func))))


(defmethod transform-to-expr ((expr ir1:let)
                              &key
                                current-continuation
                                lexenv
                              &allow-other-keys)
  (let* ((old-def (ir1:def expr))
         (var-name (ir1:name old-def))
         (var-type (transform-type (ir1:type old-def)))
         (var (make-instance 'local
                             :name var-name
                             :type var-type))
         (bound-env (cons var lexenv))
         (body (transform-to-expr (ir1:body expr)
                                  :current-continuation current-continuation
                                  :lexenv bound-env))
         (binding-form (transform-binding (ir1:initform old-def)
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
      (transform-to-var (ir1:predicate expr) :lexenv lexenv)
    (let* ((then-expr
             (transform-to-expr (ir1:then-case expr)
                                :current-continuation current-continuation
                                :lexenv lexenv))
           (else-expr
             (transform-to-expr (ir1:else-case expr)
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
  (let* ((ret-expr (transform-to-expr (ir1:return-value expr)
                                      :current-continuation current-continuation
                                      :lexenv lexenv))
         (ignore-cont-name (make-continuation-arg 'prog2
                                                  (transform-type (ir1:type (ir1:side-effect expr)))))
         (side-effect-expr (transform-to-expr (ir1:side-effect expr)
                                              :current-continuation ignore-cont-name
                                              :lexenv lexenv))
         (ignore-var (make-instance 'local
                                    :name (make-gensym 'ignore)
                                    :type (transform-type
                                           (ir1:type
                                            (ir1:side-effect expr)))))
         (ignore-arglist (specialized-vector variable ignore-var))
         (ignore-cont (make-instance 'proc
                                     :name ignore-cont-name
                                     :arglist ignore-arglist
                                     :body ret-expr
                                     :in side-effect-expr)))
    ignore-cont))

(defmethod transform-to-expr ((expr ir1:expr)
                              &key
                                current-continuation
                                lexenv
                              &allow-other-keys)
  (let* ((var (make-instance 'local
                             :name (make-gensym 'tmp)
                             :type (transform-type (ir1:type expr))))
         (body (make-instance 'apply
                              :args (specialized-vector variable var)
                              :func current-continuation)))
    (transform-binding expr var body :lexenv lexenv)))

;;; transform-to-var methods

(defmethod transform-to-var ((expr ir1:variable)
                             &key
                               lexenv
                             &allow-other-keys)
  (find-variable (ir1:name expr) lexenv))

(defmethod transform-to-var ((expr ir1:quote)
                             &key &allow-other-keys)
  (make-instance 'constant
                 :type (transform-type (ir1:type expr))
                 :value (ir1:it expr)))

(defmethod transform-to-var ((expr ir1:expr)
                             &key
                             &allow-other-keys)
  (let* ((variable (make-instance 'local
                                  :name (make-gensym 'var)
                                  :type (transform-type (ir1:type expr)))))
    (values variable (acons variable expr ()))))

;;; transform-binding methods

(|:| #'lambda-arg-vars (-> (ir1:lambda) (vector variable)))
(defun lambda-arg-vars (ir1-lambda)
  (iter
    (for arg-name in-vector (ir1:bindings ir1-lambda))
    (for arg-type in-vector (ir1:inputs (ir1:type ir1-lambda)))
    (collect (make-instance 'local
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
  (let* ((continuation-arg (make-continuation-arg 'return
                                                  (transform-type (ir1:output (ir1:type initform)))))
         (normal-args (lambda-arg-vars initform))
         (arglist (concatenate '(vector variable)
                               (list continuation-arg)
                               normal-args))
         (fenv (augment-lexenv-for-func lexenv arglist))
         (fbody (transform-to-expr (ir1:body initform)
                                   :current-continuation continuation-arg
                                   :lexenv fenv)))
    (make-instance 'proc
                   :name var
                   :arglist arglist
                   :body fbody
                   :in body)))

(defmethod transform-binding ((initform ir1:primop) var body
                              &key lexenv &allow-other-keys)
  (multiple-value-bind (arg-vars arg-terms)
      (arg-vec-vars-and-terms (ir1:args initform) lexenv)
    (let* ((let-expr (make-instance 'let
                                    :var var
                                    :prim-op (ir1:op initform)
                                    :args arg-vars
                                    :in body)))
      (compute-intermediate-terms arg-terms
                                  let-expr
                                  lexenv))))

(defmethod transform-binding ((initform ir1:quote) var body &key &allow-other-keys)
  (subst (transform-to-var initform) var body))

(defmethod transform-binding ((initform ir1:variable) var body &key &allow-other-keys)
  (subst (transform-to-var initform) var body))

(defmethod transform-binding ((initform ir1:expr) var body
                              &key
                                lexenv
                              &allow-other-keys)
  (let* ((cont-name (make-continuation-arg 'cont
                                           (transform-type (ir1:type initform))))
         (arglist (specialized-vector variable var))
         (lexenv (augment-lexenv-for-func lexenv arglist))
         (expr (transform-to-expr initform
                                  :lexenv lexenv
                                  :current-continuation cont-name)))
    (make-instance 'proc
                   :name cont-name
                   :arglist arglist
                   :body body
                   :in expr)))

(defvar *exit-continuation* (make-instance 'local
                                           :name 'exit
                                           :type (make-instance 'function
                                                                :inputs (specialized-vector repr-type *fixnum*)))
  "the continuation to exit the program")

(defun cps-transform (typed-ir1-program)
  (iter
    (if-first-time (collect *exit-continuation* into lexenv at beginning))
    (for def in-vector (ir1:definitions typed-ir1-program))
    (for local = (make-instance 'local
                                 :name (ir1:name def)
                                 :type (transform-type (ir1:type def))))
    (collect (cons local (ir1:initform def))
      into intermediate-terms
      at beginning)
    (collect local into lexenv at beginning)
    (finally
     (return
       (compute-intermediate-terms intermediate-terms
                                   (transform-to-expr (ir1:entry typed-ir1-program)
                                                      :lexenv lexenv
                                                      :current-continuation *exit-continuation*)
                                   lexenv)))))
