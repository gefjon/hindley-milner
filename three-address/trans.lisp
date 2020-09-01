(uiop:define-package :hindley-milner/three-address/trans
  (:mix
   :hindley-milner/three-address/type
   :hindley-milner/three-address/expr
   :hindley-milner/prologue
   :iterate
   :cl)
  (:import-from :hindley-milner/cps)
  (:import-from :alexandria
   :eswitch)
  (:export
   :three-address-transform-program))
(cl:in-package :hindley-milner/three-address/trans)

(defgeneric extend-type (type)
  (:method ((type cps:type-variable))
    *opaque-ptr*)
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
                     :fptr fptr)))
  (:method ((type cps:struct))
    (make-instance 'struct
                   :elts (map '(vector repr-type) #'extend-type
                              (cps:elts type))))
  (:method ((type cps:enum))
    (iter (for cps-variant in-vector (cps:variants type))
      (for variant = (extend-type cps-variant))
      (finding variant maximizing (size type) into biggest)
      (finally (return
                 (make-instance 'struct
                                :elts (specialized-vector repr-type
                                                          *fixnum*
                                                          biggest)))))))

(define-special *current-procedure* procedure)
(define-special *program* program)
(define-special *closure-idxes* (hash-map-of symbol index))
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
  (or (gethash (cps:name closure) *closure-idxes*)
      (error "~a is not in the closure env for ~a" closure (name *current-procedure*))))

(defgeneric read-from (var)
  (:documentation "transform a load from VAR into a `register', possibly mutating `*current-procedure*' along the way")
  (:method ((var cps:local))
    (read-from (local-for-var var)))
  (:method ((var cps:closure))
    (let ((reg (local-for-var var)))
      (insn 'read-struct
            :dst reg
            :src *current-closure-env*
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
     (-> (global (or null (vector local)) &optional (or null cps:closure-vars))
         (values procedure basic-block local)))
(defun make-procedure (name arglist &optional closure-env)
  "Returns as multiple values a `procedure', its entry `basic-block', and the `local' which refers to its `closure-env'."
  (let* ((closure-env (make-instance 'struct
                                     :elts (map '(vector repr-type) (compose #'extend-type #'cps:type)
                                                closure-env)))
         (*current-bb* (make-instance 'basic-block
                                      :label nil
                                      :body (adjustable-vector instr)))
         (opaque-closure-arg (make-closure-arg name))
         (args (coerce (cons opaque-closure-arg
                              (map 'list #'corresponding-local
                                   arglist))
                       '(vector local)))
         (typed-closure-reg (make-closure-arg name closure-env)))
    (unless (uiop:emptyp (elts closure-env))
      (insn 'pointer-cast
            :dst typed-closure-reg
            :src opaque-closure-arg))
    (values (make-instance 'procedure
                           :name name
                           :args args
                           :closure-env closure-env
                           :body (adjustable-vector basic-block *current-bb*))
            *current-bb*
            typed-closure-reg)))

(|:| #'add-procedure (-> (procedure) global))
(defun add-procedure (proc)
  (vector-push-extend proc (procs *program*))
  (name proc))


(|:| #'make-closure-idxes (-> (sequence) (hash-map-of symbol index)))
(defun make-closure-idxes (cenv)
  (iter
    (with table = (make-hash-table :test #'eq))
    (for closure in-sequence cenv)
    (for i upfrom 0)
    (setf (gethash (cps:name closure) table) i)
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

(defmethod transform-expr ((expr cps:alloc-struct))
  (insn 'make-struct
        :dst (corresponding-local (cps:var expr))
        :elts (read-from (cps:elts expr)))
  (transform-expr (cps:in expr)))

(defmethod transform-expr ((expr cps:read-struct))
  (insn 'read-struct
        :dst (corresponding-local (cps:var expr))
        :src (read-from (cps:src expr))
        :index (cps:idx expr))
  (transform-expr (cps:in expr)))

(defmethod transform-expr ((expr cps:transmute))
  (if (cps:type-equal (cps:type (cps:new expr))
                      (cps:type (cps:old expr)))
      (transform-expr (subst (cps:new expr) (cps:old expr) (cps:in expr)))
      (progn
        (insn 'pointer-cast
              :dst (corresponding-local (cps:new expr))
              :src (read-from (cps:old expr)))
        (transform-expr (cps:in expr)))))

(defmethod transform-expr ((expr cps:let))
  (insn 'primop
        :dst (corresponding-local (cps:var expr))
        :op (cps:prim-op expr)
        :args (read-from (cps:args expr)))
  (transform-expr (cps:in expr)))

(|:| #'add-proc (-> (cps:proc) void))
(defun add-proc (defn)
  (let* ((reg (corresponding-local (cps:name defn)))
         (fname (make-instance 'global
                               :name (name reg)
                               :type (fptr (type reg))))
         (env-ty (make-instance
                  'struct
                  :elts (map '(vector repr-type)
                             (compose #'extend-type #'cps:type)
                             (cps:closes-over defn))))
         (typed-env (make-instance 'local
                                   :name (format-gensym "~a-env" (name reg))
                                   :type env-ty))
         (untyped-env (make-instance 'local
                                     :name (format-gensym "~a-env" (name reg))
                                     :type *opaque-ptr*)))
    (with-procedure
        (fname (cps:arglist defn) (cps:closes-over defn))
      (transform-expr (cps:body defn)))
    (insn 'make-struct
          :dst typed-env
          :elts (read-from
                 (map '(vector cps:local) #'cps:corresponding-local
                      (cps:closes-over defn))))
    (insn 'pointer-cast
          :dst untyped-env
          :src typed-env)
    (insn 'make-closure-func
          :dst reg
          :env untyped-env
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

(defmacro with-program (&body body)
  `(let* ((*var-locals* (make-hash-table :test #'eq)))
     (with-procedure (*main* (specialized-vector local (corresponding-local cps:*exit-continuation*)) () :add nil)
       (let* ((*program* (make-instance 'program
                                        :procs (adjustable-vector procedure)
                                        :entry *current-procedure*)))
         ,@body
         *program*))))

(defun three-address-transform-program (cps-program)
  (with-program
    (transform-expr cps-program)))
