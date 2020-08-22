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

   :type-equal

   :*i1* :*i8* :*i32* :*i64*
   :*void* :*opaque-ptr* :*double-star* :*gc-recurse-func*

   :val
   :global :name
   :local :name
   :const :val
   :undef :*undef*
   :nullptr :*null*

   :*zero* :*one*
   :*gcalloc* :*collect-and-relocate*
   
   :arith-op

   :instr
   :br :cond :if-true :if-false
   :tailcall :func :args
   :alloca :dst :type :ct-type :ct
   :c-call :dst :ret :func :args
   :bitcast :dst :in-ty :in :out-ty
   :ptrtoint :dst :in-ty :in :out-ty
   :extractvalue :dst :agg-ty :agg :indices
   :insertvalue :dst :agg-ty :agg :field-ty :field :indices
   :getelementptr :dst :agg-ty :ptr-ty :ptr :indices
   :load :dst :ty :ptr-ty :ptr
   :store :ty :ptr-ty :ptr :val
   :arith :dst :op :ty :lhs :rhs
   :ret
   :unreachable

   :basic-block :label :body
   :procedure :name :calling-convention :args :body
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

(defgeneric type-equal (lht rht)
  (:method :around (lht rht) (or (eq lht rht) (call-next-method)))
  (:method (lht rht) (declare (ignorable lht rht)) nil)
  (:method ((lht token) (rht token)) t)
  (:method ((lht void) (rht void)) t)
  (:method ((lht integer) (rht integer)) (= (bitwidth lht) (bitwidth rht)))
  (:method ((lht pointer) (rht pointer)) (type-equal (pointee lht) (pointee rht)))
  (:method ((lht struct) (rht struct)) (not (mismatch (members lht) (members rht) :test #'type-equal))))

(|:| #'pointer (-> (repr-type) pointer))
(defun pointer (pointee)
  (make-instance 'pointer :pointee pointee))

(defvar *i1* (make-instance 'integer :bitwidth 1))
(defvar *i8* (make-instance 'integer :bitwidth 8))
(defvar *i32* (make-instance 'integer :bitwidth 32))
(defvar *i64* (make-instance 'integer :bitwidth 64))

(defvar *void* (make-instance 'void))

(defvar *opaque-ptr* (pointer *i8*))
(defvar *double-star* (pointer *opaque-ptr*))

(defvar *gc-recurse-func*
  (make-instance 'pointer
                 :pointee (make-instance 'function
                                         :inputs (specialized-vector
                                                  repr-type
                                                  *opaque-ptr* ; old
                                                  *opaque-ptr* ; new
                                                  ))))

(define-enum val ()
  ((global ((name symbol)))
   (local ((name symbol)))
   (const ((val t)))
   (undef ())
   (nullptr ())))

(|:| #'const (-> (t) const))
(defun const (val)
  (make-instance 'const :val val))

(defvar *zero* (const 0))
(defvar *one* (const 1))

(defvar *gcalloc* (make-instance 'global :name '|gcalloc|))
(defvar *collect-and-relocate* (make-instance 'global :name '|collect_and_relocate|))

(defvar *undef* (make-instance 'undef))
(defvar *null* (make-instance 'nullptr))

(deftype arith-op ()
  '(member :add :sub :mul :div))

(define-enum instr ()
  ((br ((cond val)
        (if-true symbol)
        (if-false symbol)))
   (tailcall ((func val)
              (args (vector (cons repr-type val)))))
   (alloca ((dst local)
            (type repr-type)
            (ct-type integer)
            (ct val)))
   (c-call ((dst local :may-init-unbound t)
            (ret repr-type)
            (func val)
            (args (vector (cons repr-type val)))))
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
                   (indices (vector (cons repr-type val)))))
   (load ((dst local)
          (ty repr-type)
          (ptr-ty pointer)
          (ptr val)))
   (store ((ty repr-type)
           (ptr-ty pointer)
           (ptr val)
           (val val)))
   (arith ((dst local)
           (op arith-op)
           (ty repr-type)
           (lhs val)
           (rhs val)))
   (ret ())
   (unreachable ())))

(define-class basic-block
    ((label (or null symbol))
     (body (vector instr) :initform (adjustable-vector instr))))

(define-class procedure
    ((name global)
     (calling-convention (member :ccc :tailcc))
     (args (vector (cons repr-type local)))
     (body (vector basic-block))))

(define-class program
    ((procs (vector procedure))
     (entry procedure)))
