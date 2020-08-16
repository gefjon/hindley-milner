(uiop:define-package :hindley-milner/cps/type
  (:mix :hindley-milner/prologue :cl)
  (:shadow :function)
  (:export
   :primitive-type
   :*void* :*boolean* :*fixnum*
   :repr-type
   :type-variable :name
   :primitive :primitive
   :function :inputs
   :type-equal))
(in-package :hindley-milner/cps/type)

(deftype primitive-type ()
  '(member :void :boolean :fixnum))

(define-enum repr-type ()
  ((type-variable ((name symbol)))
   (primitive ((primitive primitive-type)))
   (function ((inputs (vector repr-type))))))

(deftype literal ()
  '(or (member hm:|true| hm:|false|)
    integer))

(define-primitive-types primitive
  :void :boolean :fixnum)

(defgeneric type-equal (lhs rhs)
  (:method (lhs rhs)
    (declare (ignorable lhs rhs))
    nil)
  (:method ((lhs type-variable) (rhs type-variable))
    (eq (name lhs) (name rhs)))
  (:method ((lhs primitive) (rhs primitive))
    (eq (primitive lhs) (primitive rhs)))
  (:method ((lhs function) (rhs function))
    (not (mismatch (inputs lhs) (inputs rhs) :test #'type-equal))))
