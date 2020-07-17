(uiop:define-package :hindley-milner/repr-type
  (:mix
   :hindley-milner/prologue
   :cl
   :iterate)
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
   :literal))
(cl:in-package :hindley-milner/repr-type)

(deftype primitive-type ()
  '(member :void :boolean :fixnum :byte))

(define-enum repr-type ()
  ((primitive ((primitive primitive-type)))
   (function-ptr ((inputs (vector repr-type))))
   (closure-func ((fptr function-ptr)))
   (closure-env ((elts (vector repr-type))))
   (gc-ptr ((pointee repr-type)))))

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

(defgeneric gc-ptr-indices (type)
  (:documentation "Returns `t' if TYPE is a `gc-ptr', or a tree of indices of `gc-ptr's within it.")
  (:method ((ptr gc-ptr))
    (declare (ignorable ptr))
    t)
  (:method ((closure closure-func))
    (declare (ignorable closure))
    '(1))
  (:method ((prim primitive))
    (declare (ignorable prim))
    nil)
  (:method ((fptr function-ptr))
    (declare (ignorable fptr))
    nil)
  (:method ((env closure-env))
    (iter
      (for i upfrom 0)
      (for field in-vector (elts env))
      (for idxes-in-field = (gc-ptr-indices field))
      (etypecase idxes-in-field
        (null (next-iteration))
        ((eql t) (collect (list i) into indices at beginning))
        (cons (collect (cons i idxes-in-field)
                into indices at beginning)))
      (finally (return indices)))))
