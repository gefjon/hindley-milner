(uiop:define-package :hindley-milner/three-address/trans
  (:mix
   :hindley-milner/three-address/expr
   :hindley-milner/prologue
   :iterate
   :cl)
  (:import-from :hindley-milner/cps)
  (:import-from :alexandria
   :with-gensyms :make-gensym :compose :when-let)
  (:export
   :with-procedure :with-program :insn :*current-procedure* :*program*

   :three-address-transform-program))
(cl:in-package :hindley-milner/three-address/trans)

(define-special *current-procedure* procedure)
(define-special *program* program)
(define-special *global-idxes* (hash-map-of cps:global index))
(define-special *closure-idxes* (hash-map-of cps:closure index))
(define-special *var-regs* (hash-map-of cps:variable register))
(define-special *func-closure-vars* (hash-map-of cps:variable register))
(define-special *closure-source-vars* (vector cps:variable)
    "maps closure indices to the variables they are sourced from")

(|:| #'insn (-> (symbol &rest t) void))
(defun insn (class &rest kwargs)
  (vector-push-extend (apply #'make-instance class kwargs)
                      (body *current-procedure*))
  (values))

(|:| #'reg-for-var (-> (cps:variable &optional symbol) register))
(defun reg-for-var (var &optional (type 'reg))
  (ensure-get var *var-regs*
              (make-instance type
                             :name (cps:name var)
                             :type (cps:type var))))

(|:| #'idx-for-global (-> (cps:global) register))
(defun idx-for-global (global)
  (ensure-get global *global-idxes*
              (vector-push-extend (cps:type global)
                                  (globals *program*))))

(|:| #'idx-for-closure (-> (cps:closure) index))
(defun idx-for-closure (closure)
  (or (gethash closure *closure-idxes*)
      (error "~a is not in the closure env for ~a" closure (name *current-procedure*))))

(|:| #'closure-env-for-func (-> (cps:variable) register))
(defun closure-env-for-func (func)
  (ensure-get func *func-closure-vars*
              (make-instance 'reg
                             :type :closure-env
                             :name (format-gensym "~a-closure-env" (cps:name func)))))

(defgeneric read-from (var)
  (:documentation "transform a load from VAR into a `register', possibly mutating `*current-procedure*' along the way"))

(defmethod read-from ((var cps:local))
  (reg-for-var var))

(defmethod read-from ((var cps:global))
  (let ((reg (reg-for-var var)))
    (insn 'read-global
          :dst reg
          :global (idx-for-global var))
    reg))

(defmethod read-from ((var cps:closure))
  (let ((reg (reg-for-var var)))
    (insn 'read-closure-env
          :dst reg
          :index (idx-for-closure var))
    reg))

(defmethod read-from ((var vector))
  (map '(vector register) #'read-from var))

(defgeneric store-into-and-discard (register variable)
  (:documentation "store the value currently contained in REGISTER into VARIABLE. then mark REGISTER as dead."))

(defmethod store-into-and-discard ((src register) (var cps:local))
  (let ((dst (reg-for-var var)))
    (unless (eq src dst)
      (insn 'copy
            :dst dst
            :src src)
      (insn 'dead
            :reg src))))

(defmethod store-into-and-discard ((src register) (var cps:global))
  (insn 'set-global
        :global (idx-for-global var)
        :src src)
  (insn 'dead
        :reg src))

(defmethod store-into-and-discard ((src register) (dst register))
  (unless (eq src dst)
    (insn 'copy
          :dst dst
          :src src)
    (insn 'dead
          :reg src)))

(defmacro with-dst ((reg var) &body body)
  (check-type reg symbol)
  (with-gensyms (vari)
    `(let* ((,vari ,var)
            (,reg (if (typep ,vari 'register) ,vari
                      (reg-for-var ,vari))))
       ,@body
       (store-into-and-discard ,reg ,vari))))

(defun make-procedure (name arglist &optional closure-env)
  (let* ((args (map '(vector register) #'reg-for-var
                    arglist))
         (closure-env (map '(vector type) (compose #'cps:type #'car)
                           closure-env)))
    (make-instance 'procedure
                   :name name
                   :args args
                   :closure-env closure-env
                   :body (adjustable-vector instr))))

(|:| #'add-procedure (-> (procedure) index))
(defun add-procedure (proc)
  (vector-push-extend proc (procs *program*)))

(defmacro with-procedure ((name arglist closure-env
                           &key (add t)) &body body)
  (with-gensyms (cenv)
    `(let* ((,cenv ,closure-env)
            (*current-procedure* (make-procedure ,name ,arglist ,cenv))
            (*var-regs* (make-hash-table :test #'eq))
            (*closure-idxes* (iter
                               (with table = (make-hash-table :test #'eq))
                               (for (nil . closure) in-sequence ,cenv)
                               (for i upfrom 0)
                               (setf (gethash closure table) i)
                               (finally (return table))))
            (*func-closure-vars* (make-hash-table :test #'eq)))
       ,@body
       ,@(when add
           `((add-procedure *current-procedure*))))))

(defgeneric transform-expr (cps-expr))

(|:| #'idx-for-constant (-> (t) index))
(defun idx-for-constant (value)
  (or (position value (constants *program*) :test #'eq)
      (vector-push-extend value (constants *program*))))

(defmethod transform-expr ((expr cps:let))
  (with-dst (dst (cps:var expr))
    (insn 'primop
          :dst dst
          :op (cps:prim-op expr)
          :args (read-from (cps:args expr)))))

(defgeneric add-definition (defn))

(defmethod add-definition ((defn cps:constant))
  (with-dst (dst (cps:name defn))
    (insn 'load-constant
          :dst dst
          :index (idx-for-constant (cps:value defn)))))

(defgeneric arglist (proc)
  (:method ((proc cps:func))
    (cons (cps:continuation-arg proc)
          (coerce (cps:arglist proc) 'list)))
  (:method ((proc cps:continuation))
    (specialized-vector cps:variable
                        (cps:arg proc))))

(defmethod add-definition ((defn cps:procedure))
  (with-dst (dst (cps:name defn))
    (with-procedure
        ((name dst) (arglist defn) (cps:closes-over defn))
      (transform-expr (cps:body defn))))
  (with-dst (dst (closure-env-for-func (cps:name defn)))
    (insn 'make-closure-env
          :dst dst
          :elts (read-from (map '(vector variable) #'car
                                (cps:closes-over defn))))))

(defmethod transform-expr ((expr cps:bind))
  (add-definition (cps:defn expr))
  (transform-expr (cps:in expr)))

(defmethod transform-expr ((expr cps:if))
  (let* ((pred (read-from (cps:predicate expr)))
         (then (with-procedure
                   ((make-gensym 'then) () (closure-env *current-procedure*))
                 (transform-expr (cps:then-clause expr))))
         (else (with-procedure
                   ((make-gensym 'else) () (closure-env *current-procedure*))
                 (transform-expr (cps:else-clause expr)))))
    (insn 'call
          :condition pred
          :func then)
    (insn 'call
          :condition t
          :func else)))

(defmethod transform-expr ((expr cps:apply))
  (insn 'param
        :src (read-from (cps:continuation expr)))
  (iter
    (for arg in-vector (cps:args expr))
    (insn 'param
          :src (read-from arg)))
  (when-let ((env (closure-env-for-func (cps:func expr))))
    (insn 'set-closure-env
          :src env))
  (insn 'call
        :condition t
        :func (read-from (cps:func expr))))

(defmethod transform-expr ((expr cps:throw))
  (insn 'param
        :src (read-from (cps:arg expr)))
  (when-let ((env (closure-env-for-func (cps:cont expr))))
    (insn 'set-closure-env
          :src env))
  (insn 'call
        :condition t
        :func (read-from (cps:cont expr))))

(defmacro with-program (&body body)
  `(with-procedure ('main () () :add nil)
     (let* ((*program* (make-instance 'program
                                    :procs (adjustable-vector procedure)
                                    :globals (adjustable-vector type)
                                    :entry *current-procedure*
                                    :constants (adjustable-vector t)))
            (*global-idxes* (make-hash-table :test #'eq)))
     ,@body
     *program*)))

(defun three-address-transform-program (cps-program)
  (with-program
    (transform-expr cps-program)))
