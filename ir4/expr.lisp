(uiop:define-package :hindley-milner/ir4/expr
  (:mix
   :hindley-milner/prologue
   :cl)
  (:shadow
   :integer :function :cond :type)
  (:export
   :index

   :repr-type
   :token
   :integer :bitwidth
   :pointer :pointee
   :function :inputs :output
   :pointer :pointee
   :struct :members

   :*i1* :*i8* :*i32* :*i64*
   
   :val
   :global :name
   :local :name
   :const :val
   :undef :*undef*
   :nullptr :*null*

   :arith-op

   :instr
   :br :cond :if-true :if-false
   :tailcall :func :args
   :alloc-call :dst :arg :live-ptrs
   :alloc-relocate :dst :type :token :index
   :alloc-result :dst :type :token
   :bitcast :dst :in-ty :in :out-ty
   :ptrtoint :dst :in-ty :in :out-ty
   :extractvalue :dst :agg-ty :agg :indices
   :insertvalue :dst :agg-ty :agg :field-ty :field :indices
   :getelementptr :dst :agg-ty :ptr-ty :ptr :indices
   :load :dst :ty :ptr-ty :ptr
   :arith :dst :op :ty :lhs :rhs
   :ret

   :basic-block :label :body
   :procedure :name :args :body
   :program :procs :entry))
(in-package :hindley-milner/ir4/expr)

(deftype index ()
  '(unsigned-byte 32))

(define-enum repr-type ()
  ((token ())
   (void ())
   (integer ((bitwidth index)))
   (function ((inputs (vector repr-type))
              (output repr-type :initform (make-instance 'void))))
   (pointer ((pointee repr-type)))
   (struct ((members (vector repr-type))))))

(defvar *i1* (make-instance 'integer :bitwidth 1))
(defvar *i8* (make-instance 'integer :bitwidth 8))
(defvar *i32* (make-instance 'integer :bitwidth 32))
(defvar *i64* (make-instance 'integer :bitwidth 64))

(define-enum val ()
  ((global ((name symbol)))
   (local ((name symbol)))
   (const ((val t)))
   (undef ())
   (nullptr ())))

(defvar *undef* (make-instance 'undef))
(defvar *null* (make-instance 'nullptr))

(deftype arith-op ()
  '(member :add :sub :mul :div))

(define-enum instr ()
  ((br ((cond local)
        (if-true symbol)
        (if-false symbol)))
   (tailcall ((func val)
              (args (vector (cons repr-type val)))))
   (alloc-call ((dst local)
                (arg (cons repr-type val))
                (live-ptrs (vector (cons repr-type local)))))
   (alloc-relocate ((dst local)
                    (type repr-type)
                    (token local)
                    (index index)))
   (alloc-result ((dst local)
                  (type repr-type)
                  (token local)))
   (bitcast ((dst local)
             (in-ty repr-type)
             (in val)
             (out-ty repr-type)))
   (ptrtoint ((dst local)
              (in-ty pointer)
              (in val)
              (out-ty integer)))
   (extractvalue ((dst local)
                  (agg-ty repr-type)
                  (agg val)
                  (indices (vector index))))
   (insertvalue ((dst local)
                 (agg-ty repr-type)
                 (agg val)
                 (field-ty repr-type)
                 (field val)
                 (indices (vector index))))
   (getelementptr ((dst local)
                   (agg-ty repr-type)
                   (ptr-ty pointer)
                   (ptr val)
                   (indices (vector (cons integer val)))))
   (load ((dst local)
          (ty repr-type)
          (ptr-ty pointer)
          (ptr val)))
   (arith ((dst local)
           (op arith-op)
           (ty repr-type)
           (lhs val)
           (rhs val)))
   (ret ())))

(define-class basic-block
    ((label (or null symbol))
     (body (vector instr) :initform (adjustable-vector instr))))

(define-class procedure
    ((name global)
     (args (vector (cons repr-type local)))
     (body (vector basic-block))))

(define-class program
    ((procs (vector procedure))
     (entry procedure)))
