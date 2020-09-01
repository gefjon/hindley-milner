(uiop:define-package :hindley-milner/three-address/type
  (:mix :hindley-milner/prologue :cl :iterate)
  (:export
   :primitive-type
   :*void* :*boolean* :*fixnum* :*opaque-ptr*
   :repr-type :size :align
   :primitive :primitive
   :function-ptr :inputs
   :closure-func :fptr
   :struct :elts))
(in-package :hindley-milner/three-address/type)

(deftype primitive-type ()
  '(member :void :boolean :fixnum :opaque-ptr))

(deftype size-t ()
  '(and unsigned-byte fixnum))

(define-enum repr-type ((size size-t :may-init-unbound t)
                        (align size-t :may-init-unbound t))
  ((primitive ((primitive primitive-type)))
   (function-ptr ((inputs (vector repr-type))
                  (size size-t :initform 8)
                  (align size-t :initform 8)))
   (closure-func ((fptr function-ptr)
                  (size size-t :initform 16)
                  (align size-t :initform 8)))
   (struct ((elts (vector repr-type))))))

(defmethod slot-unbound (class (instance struct) (slot-name (eql 'size)))
  (declare (ignorable class slot-name))
  (with-slot-accessors (size elts) instance
    (setf size 0)
    (iter 
      (for slot in-vector elts)
      (for align = (align slot))
      (for off-by = (rem size align))
      (unless (zerop off-by)
        (incf size (- align off-by)))
      (incf size (size slot))
      (finally
       (return size)))))

(defmethod slot-unbound (class (instance struct) (slot-name (eql 'align)))
  (declare (ignorable class slot-name))
  (setf (align instance)
        (align (aref (elts instance) 0))))

(define-primitive-types primitive
  (:void 0 0)
  (:boolean 1 1)
  (:fixnum 8 8)
  (:opaque-ptr 8 8))
