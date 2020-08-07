(uiop:define-package :hindley-milner/three-address/trans
  (:mix
   :hindley-milner/three-address/type
   :hindley-milner/three-address/expr
   :hindley-milner/prologue
   :iterate
   :cl)
  (:import-from :hindley-milner/cps)
  (:import-from :alexandria
   :with-gensyms :make-gensym :compose :when-let :eswitch)
  (:export
   :three-address-transform-program))
(cl:in-package :hindley-milner/three-address/trans)

(defgeneric extend-type (type)
  (:method ((type cps:primitive))
    (eswitch (type)
      (cps:*void* *void*)
      (cps:*fixnum* *fixnum*)
      (cps:*boolean* *boolean*)))
  (:method ((type cps:function))
    (let* ((normal-args (map '(vector repr-type) #'extend-type
                             (cps:inputs type)))
           (all-args (concatenate '(vector repr-type)
                                  (list *opaque-ptr*)
                                  normal-args))
           (fptr (make-instance 'function-ptr
                                :inputs all-args)))
      (make-instance 'closure-func
                     :fptr fptr))))

(define-special *current-procedure* procedure)
(define-special *program* program)
(define-special *closure-idxes* (hash-map-of cps:closure index))
(define-special *var-locals* (hash-map-of cps:variable local))
(define-special *current-bb* basic-block)
(define-special *current-closure-env* local)

(|:| #'current-closure-env (-> () register))
(defun current-closure-env ()
  (aref (args *current-procedure*) 0))

(|:| #'insn (-> (symbol &rest t) void))
(defun insn (class &rest kwargs)
  (vector-push-extend (apply #'make-instance class kwargs)
                      (body *current-bb*))
  (values))

(defun local-for-var (var)
  (ensure-get var *var-locals*
              (make-instance 'local
                                :name (cps:name var)
                                :type (extend-type (cps:type var)))))

(defgeneric corresponding-local (var)
  (:method ((var local)) var)
  (:method ((var cps:variable)) (local-for-var var)))

(|:| #'idx-for-closure (-> (cps:closure) index))
(defun idx-for-closure (closure)
  (or (gethash closure *closure-idxes*)
      (error "~a is not in the closure env for ~a" closure (name *current-procedure*))))

(defgeneric read-from (var)
  (:documentation "transform a load from VAR into a `register', possibly mutating `*current-procedure*' along the way")
  (:method ((var cps:local))
    (read-from (local-for-var var)))
  (:method ((var cps:closure))
    (let ((reg (local-for-var var)))
      (insn 'read-closure-env
            :dst reg
            :env *current-closure-env*
            :index (idx-for-closure var))
      reg))
  (:method ((var cps:constant))
    (make-instance 'constant
                   :type (extend-type (cps:type var))
                   :value (cps:value var)))
  (:method ((src register))
    src)
  (:method ((vec vector))
    (map '(vector register) #'read-from vec))
  (:method ((list list))
    (mapcar #'read-from list)))

(defun make-closure-arg (fname &optional (type *opaque-ptr*))
  (make-instance 'local
                 :name (format-gensym "~a-closure-arg" (name fname))
                 :type type))

(|:| #'make-procedure
     (-> (symbol (optional (vector register)) &optional (optional cps:closure-vars))
         (values procedure basic-block local)))
(defun make-procedure (name arglist &optional closure-env)
  "Returns as multiple values a `procedure', its entry `basic-block', and the `local' which refers to its `closure-env'."
  (let* ((closure-env (make-instance 'closure-env
                                     :elts (map '(vector type) (compose #'extend-type #'cps:type)
                                                closure-env)))
         (*current-bb* (make-instance 'basic-block
                                      :label nil
                                      :body (adjustable-vector instr)))
         (opaque-closure-arg (make-closure-arg name))
         (args (coerce (cons opaque-closure-arg
                              (map 'list #'corresponding-local
                                   arglist))
                       '(vector register)))
         (typed-closure-reg (make-closure-arg name
                                              (make-instance 'gc-ptr
                                                             :pointee closure-env))))
    (insn 'pointer-cast
          :dst typed-closure-reg
          :src opaque-closure-arg)
    (values (make-instance 'procedure
                           :name name
                           :args args
                           :closure-env closure-env
                           :body (adjustable-vector basic-block *current-bb*))
            *current-bb*
            typed-closure-reg)))

(|:| #'add-procedure (-> (procedure) index))
(defun add-procedure (proc)
  (vector-push-extend proc (procs *program*)))

(|:| #'make-closure-idxes (-> (sequence) (hash-map-of cps:closure index)))
(defun make-closure-idxes (cenv)
  (iter
    (with table = (make-hash-table :test #'eq))
    (for closure in-sequence cenv)
    (for i upfrom 0)
    (setf (gethash closure table) i)
    (finally (return table))))

(defmacro with-procedure ((name arglist closure-env
                           &key (add t)) &body body)
  (with-gensyms (cenv)
    `(let* ((,cenv ,closure-env))
       (multiple-value-bind (*current-procedure* *current-bb* *current-closure-env*)
           (make-procedure ,name ,arglist ,cenv)
         (let* ((*closure-idxes* (make-closure-idxes ,cenv)))
           ,@body
           ,@(when add
               `((add-procedure *current-procedure*))))))))

(defgeneric transform-expr (cps-expr))

(defmethod transform-expr ((expr cps:let))
  (insn 'primop
        :dst (cps:var expr)
        :op (cps:prim-op expr)
        :args (read-from (cps:args expr))))

(|:| #'add-proc (-> (cps:proc) void))
(defun add-proc (defn)
  (let* ((reg (corresponding-local (cps:name defn)))
         (fname (make-instance 'global
                               :name (name reg)
                               :type (fptr (type reg))))
         (env-ty (make-instance
                  'closure-env
                  :elts (map '(vector repr-type)
                             (compose #'extend-type #'cps:type)
                             (cps:closes-over defn))))
         (ptr-to-env (make-instance 'gc-ptr :pointee env-ty))
         (env (make-instance 'local
                             :name (format-gensym "~a-env" (name reg))
                             :type ptr-to-env)))
    (with-procedure
        (fname (cps:arglist defn) (cps:closes-over defn))
      (transform-expr (cps:body defn)))
    (insn 'make-closure-env
          :dst env
          :elts (read-from
                 (map '(vector cps:local) #'corresponding-local
                      (cps:closes-over defn))))
    (insn 'make-closure-func
          :dst reg
          :env env
          :func fname)))

(defmethod transform-expr ((expr cps:proc))
  (add-proc expr)
  (transform-expr (cps:in expr)))

(|:| #'add-bb (-> (basic-block) void))
(defun add-bb (bb)
  (vector-push-extend bb (body *current-procedure*))
  (values))

(defmacro with-bb (label &body body)
  "Execute BODY with `*current-bb*' bound to the basic block named LABEL. Return as a primary value LABEL, and as a secondary value the result of BODY."
  (with-gensyms (lbl)
    `(let* ((,lbl ,label)
            (*current-bb* (make-instance 'basic-block
                                         :label ,lbl
                                         :body (adjustable-vector instr))))
       (add-bb *current-bb*)
       (values ,lbl (progn ,@body)))))

(defmethod transform-expr ((expr cps:if))
  (let* ((pred (read-from (cps:predicate expr)))
         (then (with-bb (make-gensym 'then)
                 (transform-expr (cps:then-clause expr))))
         (else (with-bb (make-gensym 'else)
                 (transform-expr (cps:else-clause expr)))))
    (insn 'branch
          :condition pred
          :if-true then
          :if-false else)))

(defmethod transform-expr ((expr cps:apply))
  (let* ((closure-func (read-from (cps:func expr)))
         (fptr (make-instance 'local
                              :name (format-gensym "call-~a-func" (name closure-func))
                              :type (fptr (type closure-func))))
         (cenv (make-instance 'local
                              :name (format-gensym "call-~a-env" (name closure-func))
                              :type *opaque-ptr*)))
    (insn 'extract-func
          :dst fptr
          :src closure-func)
    (insn 'extract-env
          :dst cenv
          :src closure-func)
    (insn 'call
          :func fptr
          :args (concatenate '(vector local)
                             (list cenv)
                             (read-from (cps:args expr))))))

(defparameter *main* (make-instance 'global
                              :name '|hm_main|
                              :type (make-instance 'function-ptr :inputs (vector *opaque-ptr*
                                                                                 (extend-type (cps:type hindley-milner/cps/trans:*exit-continuation*))))))

(defmacro with-program (&body body)
  `(let* ((*var-locals* (make-hash-table :test #'eq)))
     (with-procedure (*main* (specialized-vector cps:local hindley-milner/cps/trans:*exit-continuation*) () :add nil)
       (let* ((*program* (make-instance 'program
                                        :procs (adjustable-vector procedure)
                                        :entry *current-procedure*)))
         ,@body
         *program*))))

(defun three-address-transform-program (cps-program)
  (with-program
    (transform-expr cps-program)))
