(uiop:define-package :hindley-milner/three-address/type
  (:mix :hindley-milner/prologue :cl)
  (:import-from :alexandria :symbolicate)
  (:export
   :primitive-type
   :*void* :*boolean* :*fixnum* :*opaque-ptr* :*byte*
   :repr-type
   :primitive :primitive
   :function-ptr :inputs
   :closure-func :fptr
   :closure-env :elts
   :gc-ptr :pointee))
(in-package :hindley-milner/three-address/type)

(deftype primitive-type ()
  '(member :void :boolean :fixnum :byte))

(define-enum repr-type ()
  ((primitive ((primitive primitive-type)))
   (function-ptr ((inputs (vector repr-type))))
   (closure-func ((fptr function-ptr)))
   (closure-env ((elts (vector repr-type))))
   (gc-ptr ((pointee repr-type)))))

(define-primitive-types primitive :void :boolean :fixnum :byte)

(defvar *opaque-ptr* (make-instance 'gc-ptr :pointee *byte*))
