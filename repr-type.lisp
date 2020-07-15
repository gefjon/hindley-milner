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
   :function-ptr :inputs
   :closure-func :fptr
   :closure-env :elts
   :gc-ptr :pointee
   :stack-ptr :pointee
   :literal
   :contains-gc-ptr-p))
(cl:in-package :hindley-milner/repr-type)

(deftype primitive-type ()
  '(member :void :boolean :fixnum :byte))

(define-enum repr-type ()
  ((primitive ((primitive primitive-type)))
   (function-ptr ((inputs (vector repr-type))))
   (closure-func ((fptr function-ptr)))
   (closure-env ((elts (vector repr-type))))
   (gc-ptr ((pointee repr-type)))
   (stack-ptr ((pointee repr-type)))))

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

(defparameter *opaque-ptr* (make-instance 'gc-ptr :pointee *byte*))

(defgeneric contains-gc-ptr-p (type)
  (:method ((ptr gc-ptr))
    (declare (ignorable ptr))
    t)
  (:method ((closure closure-func))
    (declare (ignorable closure))
    t)
  (:method ((prim primitive))
    (declare (ignorable prim))
    nil)
  (:method ((fptr function-ptr))
    (declare (ignorable fptr))
    nil)
  (:method ((ptr stack-ptr))
    (declare (ignorable ptr))
    nil)
  (:method ((env closure-env))
    (reduce #'(lambda (bool elt) (or bool (contains-gc-ptr-p elt)))
            (elts env))))
