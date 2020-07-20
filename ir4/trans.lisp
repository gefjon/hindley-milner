(uiop:define-package :hindley-milner/ir4/trans
  (:mix
   :hindley-milner/prologue
   :hindley-milner/ir4/expr
   :cl
   :iterate)
  (:import-from :hindley-milner/three-address))
(in-package :hindley-milner/ir4/trans)

(defgeneric ir4-transform (obj))
(defgeneric transform-to-val (obj))
(defgeneric transform-to-type (obj))
(defgeneric transform-instr (instr))

(defun transform-to-arg (obj)
  (cons (transform-to-type obj)
        (transform-to-val obj)))

(defmethod ir4-transform ((obj 3adr:program))
  (make-instance 'program
                 :procs (map '(vector procedure) #'ir4-transform (3adr:procs obj))
                 :entry (ir4-transform (3adr:entry obj))))

(defmethod ir4-transform ((obj 3adr:procedure))
  (make-instance 'procedure
                 :name (transform-to-val (3adr:name obj))
                 :args (map '(vector (cons repr-type local)) #'transform-to-arg (3adr:args obj))
                 :body (map '(vector basic-block) #'ir4-transform (3adr:body obj))))

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
  (make-instance 'local
                 :name (3adr:name local)))

(defmethod transform-to-val ((global 3adr:global))
  (make-instance 'global
                 :name (3adr:name global)))

(defmethod transform-to-val ((const 3adr:constant))
  (make-instance 'const
                 :val (3adr:value const)))

(defmethod transform-to-type ((global 3adr:global))
  (make-instance 'pointer
                 :pointee (transform-to-type (3adr:type global))))

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
             :indices (specialized-vector (cons integer val) (cons *i32* 3adr:index)))
      (instr 'load
             :dst (transform-to-val 3adr:dst)
             :ty (pointee ptr-ty)
             :ptr-ty ptr-ty
             :ptr elementptr))))

(defmethod transform-instr ((instr 3adr:make-closure-env))
  (error "TODO! make closure env!"))

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
           :args (map '(vector val) #'transform-to-val 3adr:args))))

(defmethod transform-instr ((instr 3adr:dead))
  (declare (ignorable instr))
  (values))
