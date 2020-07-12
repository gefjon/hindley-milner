(uiop:define-package :hindley-milner/repr-type
  (:mix
   :hindley-milner/prologue
   :cl)
  (:import-from :alexandria :symbolicate)
  (:shadow :function)
  (:export
   :primitive-type
   :*void* :*boolean* :*fixnum* :*opaque-ptr* :*byte*
   :repr-type
   :primitive :primitive
   :function :inputs
   :closure-func :fptr
   :closure-env :elts
   :pointer :pointee
   :literal))
(cl:in-package :hindley-milner/repr-type)

(deftype primitive-type ()
  '(member :void :boolean :fixnum :byte))

(define-enum repr-type ()
  ((primitive ((primitive primitive-type)))
   (function ((inputs (vector repr-type))))
   (closure-func ((fptr pointer)))
   (closure-env ((elts (vector repr-type))))
   (pointer ((pointee repr-type)))))

(deftype literal ()
  '(or (member hm:|true| hm:|false|)
    integer))

(defmacro define-primitives (&rest names)
  (flet ((defprim (name)
           (check-type name symbol)
           `(defvar ,(symbolicate '* (symbol-name name) '*)
              (make-instance 'primitive
                             :primitive ,name))))
    `(progn ,@(mapcar #'defprim names))))

(define-primitives :void :boolean :fixnum :byte)

(defvar *opaque-ptr* (make-instance 'pointer :pointee *byte*))
