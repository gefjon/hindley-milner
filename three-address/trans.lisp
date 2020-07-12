(uiop:define-package :hindley-milner/three-address/trans
  (:mix
   :hindley-milner/three-address/expr
   :hindley-milner/prologue
   :hindley-milner/repr-type
   :iterate
   :cl)
  (:import-from :hindley-milner/cps)
  (:import-from :alexandria
   :with-gensyms :make-gensym :compose :when-let)
  (:export
   :three-address-transform-program))
(cl:in-package :hindley-milner/three-address/trans)

(defgeneric extend-type (type)
  (:method ((type primitive))
    "Base case: leave `primitive' types untouched."
    type)
  (:method ((type function))
    "Interesting case: add an `*opaque-ptr*' closure-env arg to `function's."
    (make-instance 'function
                   :inputs (concatenate '(vector repr-type)
                                        (list *opaque-ptr*)
                                        (map '(vector repr-type) #'extend-type
                                             (inputs type)))))
  (:method ((type repr-type))
    "Recursive case: recurse on all other types."
    (map-slots #'extend-type type)))

(defun pointer (pointee)
  (make-instance 'pointer
                 :pointee pointee))

(define-special *current-procedure* procedure)
(define-special *program* program)
(define-special *closure-idxes* (hash-map-of cps:closure index))
(define-special *var-locals* (hash-map-of cps:variable local))
(define-special *current-bb* basic-block)
(define-special *var-globals* (hash-map-of cps:global global))
(define-special *global-locals* (hash-map-of global local))
(define-special *current-closure-env* local)

(|:| #'current-closure-env (-> () register))
(defun current-closure-env ()
  (aref (args *current-procedure*) 0))

(|:| #'insn (-> (symbol &rest t) void))
(defun insn (class &rest kwargs)
  (vector-push-extend (apply #'make-instance class kwargs)
                      (body *current-bb*))
  (values))

(|:| #'reg-type (-> ((or cps:variable register)) (member local global)))
(defun reg-type (var)
  (etypecase var
    ((or global cps:global) 'global)
    ((or local cps:local cps:closure) 'local)))

(|:| #'local-for-var (-> (cps:variable) local))
(defun local-for-var (var)
  (ensure-get var *var-locals*
              (etypecase var
                (cps:global (local-for-global (global-for-var var)))
                ((or cps:local cps:closure)
                 (make-instance 'local
                                :name (cps:name var)
                                :type (extend-type (cps:type var)))))))

(|:| #'global-for-var (-> (cps:global) global))
(defun global-for-var (var)
  (ensure-get var *var-globals*
              (make-instance 'global
                             :name (cps:name var)
                             :type (extend-type (cps:type var)))))

(|:| #'local-for-global (-> (global) local))
(defun local-for-global (global)
  (ensure-get global *global-locals*
              (make-instance 'local
                             :name (name global)
                             :type (type global))))

(defgeneric corresponding-local (var)
  (:method ((var local)) var)
  (:method ((var global)) (local-for-global var))
  (:method ((var cps:variable)) (local-for-var var)))

(|:| #'idx-for-closure (-> (cps:closure) index))
(defun idx-for-closure (closure)
  (or (gethash closure *closure-idxes*)
      (error "~a is not in the closure env for ~a" closure (name *current-procedure*))))

(defgeneric read-from (var)
  (:documentation "transform a load from VAR into a `register', possibly mutating `*current-procedure*' along the way")
  (:method ((var cps:local))
    (read-from (local-for-var var)))
  (:method ((var cps:global))
    (read-from (global-for-var var)))
  (:method ((var cps:closure))
    (let ((reg (local-for-var var)))
      (insn 'read-closure-env
            :dst reg
            :env *current-closure-env*
            :index (idx-for-closure var))
      reg))
  (:method ((src local))
    src)
  (:method ((src global)
            &aux (reg (local-for-global src)))
    (insn 'read-global
          :dst reg
          :src src)
    reg)
  (:method ((vec vector))
    (map '(vector register) #'read-from vec))
  (:method ((list list))
    (mapcar #'read-from list)))

(defgeneric store-into (src dst)
  (:documentation "store the value currently contained in SRC into DST.")
  (:method ((src local) (var cps:local))
    (store-into src (local-for-var var)))
  (:method ((src local) (var cps:global))
    (store-into src (global-for-var var)))
  (:method ((src local) (var cps:closure))
    (error "cannot store into a closure!"))
  (:method ((src local) (dst local))
    (unless (eq src dst)
      (insn 'copy
            :dst dst
            :src src)))
  (:method ((src local) (dst global))
    (insn 'set-global
          :dst dst
          :src src))
  (:method ((src global) dst)
    (store-into (read-from src) dst))
  (:method ((src cps:variable) dst)
    (store-into (read-from src) dst)))

(defmacro with-dst ((reg var) &body body)
  (check-type reg symbol)
  (with-gensyms (vari)
    `(let* ((,vari ,var)
            (,reg (corresponding-local ,vari)))
       ,@body
       (store-into ,reg ,vari))))

(defun make-closure-arg (fname &optional (type *opaque-ptr*))
  (make-instance 'local
                 :name (format-gensym "~a-closure-arg" (name fname))
                 :type type))

(defun proc-arg (var)
  (if (typep var 'cps:global)
      (make-instance 'local
                     :name (make-gensym (cps:name var))
                     :type (extend-type (cps:type var)))
      (corresponding-local var)))

(|:| #'make-procedure
     (-> (symbol (optional (vector register)) &optional (optional cps:closure-env-map))
         (values procedure basic-block local)))
(defun make-procedure (name arglist &optional closure-env)
  "Returns as multiple values a `procedure', its entry `basic-block', and the `local' which refers to its `closure-env'."
  (let* ((closure-env (make-instance 'closure-env
                                     :elts (map '(vector type) (compose #'cps:type #'car)
                                                closure-env)))
         (*current-bb* (make-instance 'basic-block
                                      :label nil
                                      :body (adjustable-vector instr)))
         (opaque-closure-arg (make-closure-arg name))
         (args (coerce (cons opaque-closure-arg
                              (map 'list #'proc-arg
                                   arglist))
                       '(vector register)))
         (typed-closure-reg (make-closure-arg name
                                              (make-instance 'pointer
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
    (for (nil . closure) in-sequence cenv)
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
  (with-dst (dst (cps:var expr))
    (insn 'primop
        :dst dst
        :op (cps:prim-op expr)
        :args (read-from (cps:args expr)))))

(defgeneric add-definition (defn))

(|:| #'maybe-add-global (-> (global &optional (optional t)) void))
(defun maybe-add-global (name &optional initform)
  (ensure-find name (globals *program*)
               (make-instance 'global-def
                              :name name
                              :initform initform)
               :key #'name))

(defmethod add-definition ((defn cps:constant)
                           &aux (name (cps:name defn)))
  (etypecase name
    (cps:global (let* ((global (global-for-var name)))
                  (maybe-add-global global
                                    (cps:value defn))
                  (insn 'read-global
                        :dst (corresponding-local global)
                        :src global)))
    (cps:local (insn 'constant
                     :dst (local-for-var name)
                     :value (cps:value defn)))))

;; The CPS representation represents non-constant global variables as
;; `cps:global' arguments to `cps:continuation's. The following
;; handful of functions (`handle-computed-global',
;; `store-computed-global' and `computed-global-p') handle
;; transforming such continuations into procedures of normal arguments
;; which store into corresponding global variables.

(|:| #'global-arg (-> (cps:procedure) (optional cps:variable)))
(defun global-arg (cps-proc)
  (with-slot-accessors (cps:arglist) cps-proc
    (and (= (length cps:arglist) 1)
         (aref cps:arglist 0))))

(|:| #'transformed-proc-global-arg (-> (procedure) local))
(defun transformed-proc-global-arg (3adr-proc)
  (with-slot-accessors (args) 3adr-proc
    (assert (= (length args) 2))
    (aref args 1)))

(|:| #'computed-global-p (-> (cps:procedure) boolean))
(defun computed-global-p (defn)
  (when-let ((arg (global-arg defn)))
    (typep arg 'cps:global)))

(|:| #'store-computed-global (-> (cps:procedure) void))
(defun store-computed-global (defn
                              &aux (global (global-for-var (global-arg defn)))
                                (local (transformed-proc-global-arg *current-procedure*)))
  (maybe-add-global global)
  (store-into local global))

(|:| #'handle-computed-global (-> (cps:procedure) void))
(defun handle-computed-global (defn)
  (when (computed-global-p defn)
    (store-computed-global defn)))

(defmethod add-definition ((defn cps:procedure))
  (let* ((reg (corresponding-local (cps:name defn)))
         (fname (make-instance 'global
                               :name (name reg)
                               :type (type reg))))
    (with-procedure
        (fname (cps:arglist defn) (cps:closes-over defn))
      (handle-computed-global defn)
      (transform-expr (cps:body defn)))
    (insn 'make-closure
          :dst reg
          :func fname
          :elts (read-from
                 (map '(vector register) #'car
                      (cps:closes-over defn))))))

(defmethod transform-expr ((expr cps:bind))
  (add-definition (cps:defn expr))
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
  (insn 'call
        :func (read-from (cps:func expr))
        :args (read-from (cps:args expr))))

(defvar *main* (make-instance 'global
                              :name 'main
                              :type (make-instance 'function
                                                   :inputs #())))

(defmacro with-program (&body body)
  `(with-procedure (*main* () () :add nil)
     (let* ((*var-locals* (make-hash-table :test #'eq))
            (*var-globals* (make-hash-table :test #'eq))
            (*global-locals* (make-hash-table :test #'eq))
            (*program* (make-instance 'program
                                      :procs (adjustable-vector procedure)
                                      :globals (adjustable-vector repr-type)
                                      :entry *current-procedure*)))
     ,@body
     *program*)))

(defun three-address-transform-program (cps-program)
  (with-program
    (transform-expr cps-program)))
