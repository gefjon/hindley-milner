(uiop:define-package :hindley-milner/ir4/trans
  (:mix
   :hindley-milner/prologue
   :hindley-milner/ir4/expr
   :cl
   :iterate)
  (:import-from :trivial-types :proper-list)
  (:import-from :uiop
   :emptyp)
  (:import-from :hindley-milner/three-address)
  (:export :ir4-transform))
(in-package :hindley-milner/ir4/trans)

(define-special *reg-locals* (hash-map-of 3adr:local val))
(define-special *globals* (hash-map-of 3adr:global global))
(define-special *procs* (adjustable-vector procedure))

(|:| #'add-proc (-> (procedure) void))
(defun add-proc (new-proc)
  (vector-push-extend new-proc *procs*)
  (values))

(defgeneric ir4-transform (obj))
(defgeneric transform-to-val (obj))
(defgeneric transform-to-type (obj))
(defgeneric transform-instr (instr))

(defun transform-to-arg (obj)
  (cons (transform-to-type obj)
        (transform-to-val obj)))

(|:| #'transform-and-add-proc (-> (procedure) void))
(defun transform-and-add-proc (proc)
  (add-proc (ir4-transform proc)))

(defmethod ir4-transform ((obj 3adr:program)
                          &aux (*globals* (make-hash-table :test 'eq))
                            (*procs* (adjustable-vector procedure)))
  (map nil #'transform-and-add-proc (3adr:procs obj))
  (make-instance 'program
                 :procs *procs*
                 :entry (ir4-transform (3adr:entry obj))))

(defmethod ir4-transform ((obj 3adr:procedure)
                          &aux (*reg-locals* (make-hash-table :test 'eq)))
  (let* ((name (transform-to-val (3adr:name obj)))
         (args (map '(vector (cons repr-type local)) #'transform-to-arg (3adr:args obj)))
         (body (map '(vector basic-block) #'ir4-transform (3adr:body obj))))
    (vector-push-extend (make-instance 'ret)
                        (body (aref body (1- (length body)))))
    (make-instance 'procedure
                   :name name
                   :calling-convention (if (eq (3adr:name obj) 3adr:*main*)
                                           :ccc :tailcc)
                   :args args
                   :body body)))

(define-special *current-bb* basic-block)

(defun instr (class &rest kwargs)
  (vector-push-extend (apply #'make-instance class kwargs)
                      (body *current-bb*))
  (values))

(defmethod ir4-transform ((obj 3adr:basic-block))
  (let* ((*current-bb* (make-instance 'basic-block
                                      :label (3adr:label obj))))
    (map 'nil #'transform-instr (3adr:body obj))
    *current-bb*))

(defmethod transform-to-val ((local 3adr:local))
  (ensure-get local *reg-locals*
              (make-instance 'local
                             :name (3adr:name local))))

(defmethod transform-to-val ((global 3adr:global))
  (ensure-get global *globals*
              (make-instance 'global
                             :name (3adr:name global))))

(defmethod transform-to-val ((const 3adr:constant))
  (const (3adr:value const)))

(defmethod transform-to-type ((reg 3adr:register))
  (transform-to-type (3adr:type reg)))

(defmethod transform-to-type ((global 3adr:global))
  (transform-to-type (3adr:type global)))

(defmethod transform-to-type ((prim 3adr:primitive))
  (ecase (3adr:primitive prim)
    (:void (make-instance 'void))
    (:boolean *i1*)
    (:byte *i8*)
    (:fixnum *i64*)))

(defmethod transform-to-type ((fptr 3adr:function-ptr))
  (pointer (make-instance 'function
                          :inputs (map '(vector repr-type) #'transform-to-type (3adr:inputs fptr)))))

(defmethod transform-to-type ((closure 3adr:closure-func))
  (make-instance 'struct
                 :members (specialized-vector repr-type
                                              (transform-to-type (3adr:fptr closure))
                                              (transform-to-type 3adr:*opaque-ptr*))))

(defmethod transform-to-type ((cenv 3adr:closure-env))
  (make-instance 'struct
                 :members (map '(vector repr-type) #'transform-to-type (3adr:elts cenv))))

(defmethod transform-to-type ((gc-ptr 3adr:gc-ptr))
  (pointer (transform-to-type (3adr:pointee gc-ptr))))

(defun gen-local (fmt-string &rest args)
  (make-instance 'local :name (apply #'format-gensym fmt-string args)))

(defmethod transform-instr ((instr 3adr:read-closure-env))
  (with-slot-accessors (3adr:dst 3adr:env 3adr:index) instr
    (let* ((type (transform-to-type 3adr:dst))
           (ptr-ty (pointer type))
           (env-ty (transform-to-type 3adr:env))
           (elementptr (gen-local "~a-elementptr-~a"
                                  (3adr:name 3adr:env)
                                  (3adr:name 3adr:dst))))
      (instr 'getelementptr
             :dst elementptr
             :agg-ty (pointee env-ty)
             :ptr-ty env-ty
             :ptr (transform-to-val 3adr:env)
             :indices (specialized-vector (cons integer val)
                                          (cons *i32* 0)
                                          (cons *i32* 3adr:index)))
      (instr 'load
             :dst (transform-to-val 3adr:dst)
             :ty (pointee ptr-ty)
             :ptr-ty ptr-ty
             :ptr elementptr))))

(define-enum saved-tmp-info ((old local)
                             (new local)
                             (type repr-type))
  ((gc-ptr-info ())
   (struct-info ((ptrs (vector (cons index saved-tmp-info)))))))

(|:| #'copy-local (-> (local) local))
(defun copy-local (old-local)
  (shallow-copy old-local
                :name (format-gensym "~a-copy" (name old-local))))

(defgeneric type-saved-tmp-info (type old)
  (:method ((type repr-type) old)
    (declare (ignorable type old))
    nil)
  (:method ((type pointer) old)
    (unless (typep (pointee type) 'function)
      (make-instance 'gc-ptr-info
                     :old old
                     :new (copy-local old)
                     :type type)))
  (:method ((type struct) old)
    (iter
      (for i upfrom 0)
      (for field in-vector (members type))
      (for field-local = (make-instance 'local
                                        :name (gensym "struct-elt")))
      (for idxes-in-field = (type-saved-tmp-info field field-local))
      (unless idxes-in-field (next-iteration))
      (collect (cons i idxes-in-field)
        result-type (vector (cons index saved-tmp-info))
        into ptrs)
      (finally
       (return
         (unless (emptyp ptrs)
           (make-instance 'struct-info
                          :old old
                          :new (copy-local old)
                          :type type
                          :ptrs ptrs)))))))

(define-special *save-restore-idx* unsigned-byte)

(defmacro post-increment (place)
  `(let* ((val ,place))
     (setf ,place (1+ val))
     val))

(|:| #'save-restore-ptr (-> (local) local))
(defun save-restore-ptr (base)
  (let* ((idx (post-increment *save-restore-idx*))
         (ptr (if (= idx 0) base
                  (gen-local "save-restore-ptr"))))
    (unless (= idx 0) 
      (instr 'getelementptr
             :dst ptr
             :agg-ty *opaque-ptr*
             :ptr-ty *double-star*
             :ptr base
             :indices (arglist (cons *i64* idx))))
    ptr))

(|:| #'opaquify (-> (val repr-type) val))
(defun opaquify (ptr type)
  (if (type-equal type *opaque-ptr*) ptr
      (let* ((opaqued (gen-local "opaquify")))
        (instr 'bitcast
               :dst opaqued
               :in-ty type
               :in ptr
               :out-ty *opaque-ptr*)
        opaqued)))

(defgeneric save (tmp-info buf)
  (:method ((list list) buf &aux (*save-restore-idx* 0))
    (mapc (rcurry #'save buf) list))
  (:method ((info gc-ptr-info) buf
            &aux (save-el-ptr (save-restore-ptr buf))
              (opaque-ptr (opaquify (old info) (type info))))
    (instr 'store
           :ty *opaque-ptr*
           :ptr-ty *double-star*
           :ptr save-el-ptr
           :val opaque-ptr))
  (:method ((info struct-info) buf)
    (iter
      (for (i . field) in-vector (ptrs info))
      (instr 'extractvalue
             :dst (old field)
             :agg-ty (type info)
             :agg (old info)
             :indices (specialized-vector index i))
      (save field buf))))

(defgeneric restore (tmp-info buf)
  (:method :after ((info saved-tmp-info) buf)
    (declare (ignorable buf))
    (setf (gethash (old info) *reg-locals*)
          (new info)))
  (:method ((list list) buf &aux (*save-restore-idx* 0))
    (mapc (rcurry #'restore buf) list))
  (:method ((info gc-ptr-info) buf
            &aux (restore-ptr (save-restore-ptr buf))
              (opaque-dst (if (type-equal (type info) *opaque-ptr*) (new info)
                              (gen-local "opaque-restore"))))
    (instr 'load
           :dst opaque-dst
           :ty *opaque-ptr*
           :ptr-ty *double-star*
           :ptr restore-ptr)
    (unless (type-equal (type info) *opaque-ptr*)
      (instr 'bitcast
             :dst (new info)
             :in-ty *opaque-ptr*
             :in opaque-dst
             :out-ty (type info))))
  (:method ((info struct-info) buf)
    (with-slot-accessors (new old ptrs type) info
      (iter
        (for ct upfrom 0)
        (for old-env initially old then new-env)
        (for new-env = (if (= ct (1- (length ptrs))) ; last-time-p
                           new
                           (gen-local "~a-copy-~a-" (name old) ct)))
        (for (i . field) in-vector ptrs)
        (restore field buf)
        (instr 'insertvalue
               :dst new-env
               :agg-ty type
               :agg old-env
               :field-ty (type field)
               :field (new field)
               :indices (specialized-vector index i))))))

(defgeneric move-and-recurse (tmp-info)
  (:method ((info struct-info))
    (iter (with ptr-ty = (pointer (type info)))
      (for (idx . subinfo) in-vector (ptrs info))
      (for gep-indices = (arglist (cons *i32* *zero*)
                                  (cons *i32* (const idx))))
      (instr 'getelementptr
             :dst (old subinfo)
             :agg-ty (type info)
             :ptr-ty ptr-ty
             :ptr (old info)
             :indices gep-indices)
      (move-and-recurse subinfo)))
  (:method ((info gc-ptr-info))
    (let* ((untyped-old (gen-local "untyped-pre-relocate"))
           (ptrty (pointer (type info)))
           (indirect-ptr (gen-local "untyped-relocate-indirect")))
      (instr 'bitcast
             :dst indirect-ptr
             :in-ty ptrty
             :in (old info)
             :out-ty *double-star*)
      (instr 'load
             :dst untyped-old
             :ty *opaque-ptr*
             :ptr-ty *double-star*
             :ptr indirect-ptr)
      (instr 'c-call
             :dst (new info)
             :ret *opaque-ptr*
             :func *collect-and-relocate*
             :args (arglist (cons *opaque-ptr* untyped-old)))
      (instr 'store
             :ty *opaque-ptr*
             :ptr-ty *double-star*
             :ptr indirect-ptr
             :val (new info)))))

(|:| #'insert-closure-element (-> (local pointer val repr-type index) void))
(defun insert-closure-element (env-ptr env-ptr-ty elt el-ty idx)
  (let* ((dst (gen-local "elementptr-~a" idx)))
    (instr 'getelementptr
           :dst dst
           :agg-ty (pointee env-ptr-ty)
           :ptr-ty env-ptr-ty
           :ptr env-ptr
           :indices (specialized-vector (cons integer val)
                                        (cons *i32* *zero*)
                                        (cons *i32* (const idx))))
    (instr 'store
           :ty el-ty
           :ptr-ty (pointer el-ty)
           :ptr dst
           :val elt))
  (values))

(defgeneric live-values-and-inputs (instr)
  (:method ((instr 3adr:make-closure-env))
    (concatenate 'list
                 (3adr:live-values instr)
                 (3adr:elts instr)))
  (:method ((instr 3adr:box))
    (cons (3adr:src instr)
          (coerce (3adr:live-values instr) 'list))))

(|:| #'live-values-list (-> (3adr:instr) (proper-list saved-tmp-info)))
(defun live-values-list (instr)
  (iter
    (for 3adr:local in (live-values-and-inputs instr))
    (for old-local = (transform-to-val 3adr:local))
    (for type = (transform-to-type 3adr:local))
    (for info = (type-saved-tmp-info type old-local))
    (when info
      (collecting info))))

(defgeneric count-gc-ptrs (saved-tmp-info)
  (:method ((info gc-ptr-info))
    (declare (ignorable info))
    1)
  (:method ((info struct-info))
    (iter (for (nil . subinfo) in-vector (ptrs info))
      (summing (count-gc-ptrs subinfo))))
  (:method ((list list))
    (iter (for info in list)
      (summing (count-gc-ptrs info)))))

(|:| #'compute-size (-> (repr-type) local))
(defun compute-size (type)
  (let* ((ptr (gen-local "sizeof-ptr"))
         (dst (gen-local "sizeof-int"))
         (ptr-ty (pointer type)))
    (instr 'getelementptr
           :dst ptr
           :agg-ty type
           :ptr-ty ptr-ty
           :ptr *null*
           :indices (specialized-vector (cons integer val)
                                        (cons *i32* *one*)))
    (instr 'ptrtoint
           :dst dst
           :in-ty ptr-ty
           :in ptr
           :out-ty *i64*)
    dst))

(|:| #'arglist (-> (&rest (cons repr-type val)) (vector (cons repr-type val))))
(defun arglist (&rest conses)
  (make-array (length conses) :element-type '(cons repr-type val)
                              :initial-contents conses))

(|:| #'fill-env (-> (local pointer (vector 3adr:local)) void))
(defun fill-env (env-ptr env-ptr-ty elts)
  (iter
    (for idx from 0)
    (for src in-vector elts)
    (insert-closure-element env-ptr env-ptr-ty
                            (transform-to-val src)
                            (transform-to-type src)
                            idx))
  (values))

(|:| #'gc-recurse-func-for (-> (pointer) (or global nullptr)))
(defun gc-recurse-func-for (env-ptr
                            &aux (env-type (pointee env-ptr))
                              (typed-src (gen-local "typed-old"))
                              (info (type-saved-tmp-info env-type typed-src)))
  (if (null info) *null*
      (let* ((fname (make-instance 'global
                                   :name (gensym "gc_recurse")))
             (untyped-src (gen-local "old"))
             (typed-dst (gen-local "new"))
             (untyped-dst (new info))
             (*current-bb* (make-instance 'basic-block
                                          :label nil))
             (proc (make-instance 'procedure
                                  :name fname
                                  :calling-convention :ccc
                                  :args (arglist (cons *opaque-ptr* untyped-src)
                                                 (cons *opaque-ptr* untyped-dst))
                                  :body (adjustable-vector basic-block *current-bb*))))
        (instr 'bitcast
               :dst typed-src
               :in-ty *opaque-ptr*
               :in untyped-src
               :out-ty env-ptr)
        (instr 'bitcast
               :dst typed-dst
               :in-ty *opaque-ptr*
               :in untyped-dst
               :out-ty env-ptr)
        (move-and-recurse info)
        (instr 'ret)
        (add-proc proc)
        fname)))

(defun allocate-and-fill-closure-env (instr)
  (with-slot-accessors (3adr:dst 3adr:elts) instr
    (let* ((infos (live-values-list instr))
           (saves-buf (gen-local "saves-buf"))
           (env-ptr-ty (transform-to-type 3adr:dst))
           (env-size (compute-size (pointee env-ptr-ty)))
           (untyped-env-ptr (transform-to-val 3adr:dst))
           (typed-env-ptr (gen-local "closure-env-~a" (3adr:name 3adr:dst)))
           (saved-ct (const (count-gc-ptrs infos))))
      (instr 'alloca
             :dst saves-buf
             :type *opaque-ptr*
             :ct-type *i64*
             :ct saved-ct)
      (save infos saves-buf)
      (instr 'c-call
             :dst untyped-env-ptr
             :ret *opaque-ptr*
             :func *gcalloc*
             :args (arglist (cons *i64* env-size)
                            (cons *gc-recurse-func* (gc-recurse-func-for env-ptr-ty))
                            (cons *double-star* saves-buf)
                            (cons *i64* saved-ct)))
      (restore infos saves-buf)
      (instr 'bitcast
             :dst typed-env-ptr
             :in-ty *opaque-ptr*
             :in untyped-env-ptr
             :out-ty env-ptr-ty)
      (fill-env typed-env-ptr env-ptr-ty 3adr:elts))))

(|:| #'make-closure-env-null (-> (3adr:local) void))
(defun make-closure-env-null (env-ptr)
  (setf (gethash env-ptr *reg-locals*)
        *null*))

(defmethod transform-instr ((instr 3adr:box))
  (with-slot-accessors (3adr:dst 3adr:src) instr
    (let* ((infos (live-values-list instr))
           (saves-buf (gen-local "saves-buf"))
           (val-ty (transform-to-type 3adr:src))
           (ptr-ty (pointer val-ty))
           (size (compute-size val-ty))
           (untyped-ptr (transform-to-val 3adr:dst))
           (typed-ptr (gen-local "box"))
           (saved-ct (const (count-gc-ptrs infos))))
      (instr 'alloca
             :dst saves-buf
             :type *opaque-ptr*
             :ct-type *i64*
             :ct saved-ct)
      (save infos saves-buf)
      (instr 'c-call
             :dst untyped-ptr
             :ret *opaque-ptr*
             :func *gcalloc*
             :args (arglist (cons *i64* size)
                            (cons *gc-recurse-func* (gc-recurse-func-for ptr-ty))
                            (cons *double-star* saves-buf)
                            (cons *i64* saved-ct)))
      (restore infos saves-buf)
      (instr 'bitcast
             :dst typed-ptr
             :in-ty *opaque-ptr*
             :in untyped-ptr
             :out-ty ptr-ty)
      (instr 'store
             :ty val-ty
             :ptr-ty ptr-ty
             :ptr typed-ptr
             ;; re-transform `src' here in case it has been relocated
             :val (transform-to-val 3adr:src)))))

(defmethod transform-instr ((instr 3adr:unbox))
  (let* ((type (transform-to-type (3adr:dst instr)))
         (ptr-type (pointer type))
         (typed-ptr (gen-local "box")))
    (instr 'bitcast
           :dst typed-ptr
           :in-ty *opaque-ptr*
           :in (transform-to-val (3adr:src instr))
           :out-ty ptr-type)
    (instr 'load
           :dst (transform-to-val (3adr:dst instr))
           :ty type
           :ptr-ty ptr-type
           :ptr typed-ptr)))

(defmethod transform-instr ((instr 3adr:make-closure-env))
  (if (emptyp (3adr:elts instr))
      (make-closure-env-null (3adr:dst instr))
      (allocate-and-fill-closure-env instr)))

(defmethod transform-instr ((instr 3adr:make-closure-func))
  (with-slot-accessors (3adr:dst 3adr:env 3adr:func) instr
    (let* ((struct-ty (transform-to-type 3adr:dst))
           (halfway (gen-local "~a-insertvalue" (3adr:name 3adr:dst)))
           (func-ty (transform-to-type 3adr:func)))
      (instr 'insertvalue
             :dst halfway
             :agg-ty struct-ty 
             :agg *undef*
             :field-ty func-ty
             :field (transform-to-val 3adr:func)
             :indices (specialized-vector index 0))
      (instr 'insertvalue
             :dst (transform-to-val 3adr:dst)
             :agg-ty struct-ty
             :agg halfway
             :field-ty (transform-to-type 3adr:*opaque-ptr*)
             :field (transform-to-val 3adr:env)
             :indices (specialized-vector index 1)))))

(defmethod transform-instr ((instr 3adr:extract-env))
  (with-slot-accessors (3adr:dst 3adr:src) instr
    (instr 'extractvalue
           :dst (transform-to-val 3adr:dst)
           :agg-ty (transform-to-type 3adr:src)
           :agg (transform-to-val 3adr:src)
           :indices (specialized-vector index 1))))

(defmethod transform-instr ((instr 3adr:extract-func))
  (with-slot-accessors (3adr:dst 3adr:src) instr
    (instr 'extractvalue
           :dst (transform-to-val 3adr:dst)
           :agg-ty (transform-to-type 3adr:src)
           :agg (transform-to-val 3adr:src)
           :indices (specialized-vector index 0))))

(defmethod transform-instr ((instr 3adr:pointer-cast))
  (with-slot-accessors (3adr:dst 3adr:src) instr
    (instr 'bitcast
           :dst (transform-to-val 3adr:dst)
           :in-ty (transform-to-type 3adr:src)
           :in (transform-to-val 3adr:src)
           :out-ty (transform-to-type 3adr:dst))))

(defun transform-op (op)
  (ecase op
    (hm:+ :add)
    (hm:- :sub)
    (hm:* :mul)
    (hm:/ :div)))

(defmethod transform-instr ((instr 3adr:primop))
  (with-slot-accessors (3adr:op 3adr:dst 3adr:args) instr
    (assert (= (length 3adr:args) 2))
    (let* ((type (transform-to-type 3adr:dst)))
      (assert (typep type 'integer))
      (assert (eq type (transform-to-type (aref 3adr:args 0))))
      (assert (eq type (transform-to-type (aref 3adr:args 1))))
      (instr 'arith
             :dst (transform-to-val 3adr:dst)
             :op (transform-op 3adr:op)
             :ty type
             :lhs (transform-to-val (aref 3adr:args 0))
             :rhs (transform-to-val (aref 3adr:args 1))))))

(defmethod transform-instr ((instr 3adr:branch))
  (with-slot-accessors (3adr:condition 3adr:if-true 3adr:if-false) instr
    (instr 'br
           :cond (transform-to-val 3adr:condition)
           :if-true 3adr:if-true
           :if-false 3adr:if-false)))

(defmethod transform-instr ((instr 3adr:call))
  (with-slot-accessors (3adr:func 3adr:args) instr
    (instr 'tailcall
           :func (transform-to-val 3adr:func)
           :args (map '(vector (cons repr-type val)) #'transform-to-arg 3adr:args))
    (instr 'unreachable)))

(defmethod transform-instr ((instr 3adr:dead))
  (declare (ignorable instr))
  (values))
