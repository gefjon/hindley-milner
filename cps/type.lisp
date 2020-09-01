(uiop:define-package :hindley-milner/cps/type
  (:mix :hindley-milner/prologue :cl)
  (:shadow :function)
  (:export
   :primitive-type :*void* :*boolean* :*fixnum*
   :repr-type
   :type-variable :name
   :primitive :primitive
   :struct :elts
   :enum :variants
   :function :inputs
   :boxed-p
   :type-equal))
(in-package :hindley-milner/cps/type)

(deftype primitive-type ()
  '(member :void :boolean :fixnum))

(define-enum repr-type ()
  ((type-variable ((name symbol)))
   (primitive ((primitive primitive-type)))
   (function ((inputs (vector repr-type))))
   (struct ((elts (vector repr-type))))
   (enum ((variants (vector struct))))))

(defun struct (&rest elts)
  (make-instance 'struct :elts (coerce elts '(vector repr-type))))

(deftype literal ()
  '(or (member hm:|true| hm:|false|)
    integer))

(define-primitive-types primitive
  :void :boolean :fixnum)

(|:| #'boxed-p (-> (repr-type) boolean))
(defun boxed-p (type)
  (typep type '(or struct enum type-variable)))

(defgeneric type-equal (lhs rhs)
  (:method (lhs rhs)
    (declare (ignorable lhs rhs))
    nil)
  (:method ((lhs type-variable) (rhs type-variable))
    (eq (name lhs) (name rhs)))
  (:method ((lhs primitive) (rhs primitive))
    (eq (primitive lhs) (primitive rhs)))
  (:method ((lhs function) (rhs function))
    (not (mismatch (inputs lhs) (inputs rhs) :test #'type-equal)))
  (:method ((lhs struct) (rhs struct))
    (or (eq lhs rhs)
        (not (mismatch (elts lhs) (elts rhs)
                       :test #'type-equal))))
  (:method ((lhs enum) (rhs enum))
    (or (eq lhs rhs)
        (not (mismatch (variants lhs) (variants rhs)
                       :test #'type-equal)))))
