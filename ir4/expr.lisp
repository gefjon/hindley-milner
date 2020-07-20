(uiop:define-package :hindley-milner/ir4/expr
  (:mix
   :hindley-milner/prologue
   :cl)
  (:shadow
   :integer :function :cond)
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

   :arith-op

   :instr
   :br :cond :if-true :if-false
   :tailcall :func :args
   :alloc-call :dst :token :base-offset
   :alloc-relocate :dst :token :index
   :alloc-result :dst :token
   :bitcast :dst :in-ty :in :out-ty
   :extractvalue :dst :agg-ty :agg :indices
   :insertvalue :dst :agg-ty :agg :field-ty :field :indices
   :getelementptr :dst :agg-ty :ptr-ty :ptr :indices
   :load :dst :ty :ptr-ty :ptr
   :arith :dst :op :ty :lhs :rhs

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
   (undef ())))

(defvar *undef* (make-instance 'undef))

(deftype arith-op ()
  '(member :add :sub :mul :div))

(define-enum instr ()
  ((br ((cond local)
        (if-true symbol)
        (if-false symbol)))
   (tailcall ((func val)
              (args (vector (cons type val)))))
   (alloc-call ((dst local)
                (args (vector (cons type val)))
                (live-ptrs (vector (cons type local)))))
   (alloc-relocate ((dst local)
                    (token local)
                    (index index)))
   (alloc-result ((dst local)
                  (token local)))
   (bitcast ((dst local)
             (in-ty repr-type)
             (in val)
             (out-ty repr-type)))
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
           (rhs val)))))

(define-class basic-block
    ((label (or null symbol))
     (body (vector instr) :initform (adjustable-vector instr))))

(define-class procedure
    ((name global)
     (args (vector (cons type local)))
     (body (vector basic-block))))

(define-class program
    ((procs (vector procedure))
     (entry procedure)))
