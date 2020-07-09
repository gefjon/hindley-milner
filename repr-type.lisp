(uiop:define-package :hindley-milner/repr-type
  (:mix
   :hindley-milner/prologue
   :cl)
  (:import-from :alexandria :symbolicate)
  (:shadow :function)
  (:export
   :primitive-type
   :*void* :*boolean* :*fixnum* :*opaque-ptr*
   :repr-type
   :primitive :primitive
   :function :inputs
   :closure-env :elts))
(cl:in-package :hindley-milner/repr-type)

(deftype primitive-type ()
  '(member :void :boolean :fixnum :opaque-ptr))

(define-enum repr-type ()
  ((primitive ((primitive primitive-type)))
   (function ((inputs (vector repr-type))))
   (closure-env ((elts (vector repr-type))))))

(defmacro define-primitives (&rest names)
  (flet ((defprim (name)
           (check-type name symbol)
           `(defvar ,(symbolicate '* (symbol-name name) '*)
              (make-instance 'primitive
                             :primitive ,name))))
    `(progn ,@(mapcar #'defprim names))))

(define-primitives :void :boolean :fixnum :opaque-ptr)
