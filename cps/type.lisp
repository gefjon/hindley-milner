(uiop:define-package :hindley-milner/cps/type
  (:mix :hindley-milner/prologue :cl)
  (:shadow :function)
  (:export
   :primitive-type
   :*void* :*boolean* :*fixnum*
   :repr-type
   :primitive :primitive
   :function :inputs))
(in-package :hindley-milner/cps/type)

(deftype primitive-type ()
  '(member :void :boolean :fixnum))

(define-enum repr-type ()
  ((primitive ((primitive primitive-type)))
   (function ((inputs (vector repr-type))))))

(deftype literal ()
  '(or (member hm:|true| hm:|false|)
    integer))

(define-primitive-types primitive
  :void :boolean :fixnum)
