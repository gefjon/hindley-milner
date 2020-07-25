(uiop:define-package :hindley-milner/ir4/trans
  (:mix
   :hindley-milner/prologue
   :hindley-milner/ir4/expr
   :cl
   :iterate)
  (:import-from :alexandria
   :rcurry)
  (:import-from :uiop
   :emptyp)
  (:import-from :hindley-milner/three-address)
  (:export :ir4-transform))
(in-package :hindley-milner/ir4/trans)

(define-special *reg-locals* (hash-map-of 3adr:local local))
(define-special *globals* (hash-map-of 3adr:global global))

(defgeneric ir4-transform (obj))
(defgeneric transform-to-val (obj))
(defgeneric transform-to-type (obj))
(defgeneric transform-instr (instr))

(defun transform-to-arg (obj)
  (cons (transform-to-type obj)
        (transform-to-val obj)))

(defmethod ir4-transform ((obj 3adr:program)
                          &aux (*globals* (make-hash-table :test 'eq)))
  (make-instance 'program
                 :procs (map '(vector procedure) #'ir4-transform (3adr:procs obj))
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
  (make-instance 'const
                 :val (3adr:value const)))

(defmethod transform-to-type ((reg 3adr:register))
  (transform-to-type (3adr:type reg)))

(defmethod transform-to-type ((prim 3adr:primitive))
  (ecase (3adr:primitive prim)
    (:void (make-instance 'void))
    (:boolean *i1*)
    (:byte *i8*)
    (:fixnum *i64*)))

(defmethod transform-to-type ((fptr 3adr:function-ptr))
  (make-instance 'pointer
                 :pointee (make-instance 'function
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
  (make-instance 'pointer
                 :pointee (transform-to-type (3adr:pointee gc-ptr))))

(defun gen-local (fmt-string &rest args)
  (make-instance 'local :name (apply #'format-gensym fmt-string args)))

(defmethod transform-instr ((instr 3adr:read-closure-env))
  (with-slot-accessors (3adr:dst 3adr:env 3adr:index) instr
    (let* ((type (transform-to-type 3adr:dst))
           (ptr-ty (make-instance 'pointer :pointee type))
           (env-ty (transform-to-type 3adr:env))
           (elementptr (gen-local "~a-elementptr-~a" (3adr:name 3adr:env) (3adr:name 3adr:dst))))
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
  (make-instance 'local
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

(defgeneric save (tmp-info)
  (:method ((info gc-ptr-info))
    (list (cons (type info) (old info))))
  (:method ((info struct-info))
    (iter
      (for (i . field) in-vector (ptrs info))
      (instr 'extractvalue
             :dst (old field)
             :agg-ty (type info)
             :agg (old info)
             :indices (specialized-vector index i))
      (appending (save field)))))

(define-special *restore-idx* unsigned-byte)

(defmacro post-increment (place)
  `(let* ((val ,place))
     (setf ,place (1+ val))
     val))

(defgeneric restore (tmp-info token)
  (:method :after ((info saved-tmp-info) token)
    (declare (ignorable token))
    (setf (gethash (old info) *reg-locals*)
          (new info)))
  (:method ((info gc-ptr-info) token)
    (instr 'alloc-relocate
           :dst (new info)
           :type (type info)
           :token token
           :index (post-increment *restore-idx*)))
  (:method ((info struct-info) token)
    (with-slot-accessors (new old ptrs type) info
      (iter
        (for ct upfrom 0)
        (for old-env initially old then new-env)
        (for new-env = (if (= ct (1- (length ptrs))) ; last-time-p
                           new
                           (gen-local "~a-copy-~a-" (name old) ct)))
        
        (for (i . field) in-vector ptrs)
        (restore field token)
        (instr 'insertvalue
               :dst new-env
               :agg-ty type
               :agg old-env
               :field-ty (type field)
               :field (new field)
               :indices (specialized-vector index i))))))

(defmethod transform-instr ((instr 3adr:make-closure-env))
  (with-slot-accessors (3adr:dst 3adr:elts 3adr:live-values) instr
    (let* ((infos (iter
                    (for 3adr:local in (concatenate 'list
                                                    3adr:live-values
                                                    3adr:elts))
                    (for old-local = (transform-to-val 3adr:local))
                    (for type = (transform-to-type 3adr:local))
                    (for info = (type-saved-tmp-info type old-local))
                    (when info
                      (collecting info))))
           (saves (iter (for info in infos)
                    (appending (save info))))
           (env-ty (transform-to-type 3adr:dst))
           (size-ptr (gen-local "sizeof-~a-ptr" (3adr:name 3adr:dst)))
           (size-int (gen-local "sizeof-~a-int" (3adr:name 3adr:dst)))
           (token (gen-local "alloc-~a-token" (3adr:name 3adr:dst))))
      (check-type env-ty pointer)
      (instr 'getelementptr
             :dst size-ptr
             :agg-ty (pointee env-ty)
             :ptr-ty env-ty
             :ptr *null*
             :indices (specialized-vector (cons integer index) (cons *i32* 1)))
      (instr 'ptrtoint
             :dst size-int
             :in-ty env-ty
             :in size-ptr
             :out-ty *i64*)
      (instr 'alloc-call
             :dst token
             :arg (cons *i64* size-int)
             :live-ptrs (coerce saves '(vector (cons repr-type local))))
      (let* ((*restore-idx* 0))
        (map nil (rcurry #'restore token) infos))
      (instr 'alloc-result
             :dst (transform-to-val 3adr:dst)
             :type (transform-to-type 3adr:dst)
             :token token))))

(defmethod transform-instr ((instr 3adr:make-closure-func))
  (with-slot-accessors (3adr:dst 3adr:env 3adr:func) instr
    (let* ((halfway (gen-local "~a-insertvalue" (3adr:name 3adr:dst)))
           (struct-ty (transform-to-type 3adr:dst))
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
           :args (map '(vector (cons repr-type val)) #'transform-to-arg 3adr:args))))

(defmethod transform-instr ((instr 3adr:dead))
  (declare (ignorable instr))
  (values))
