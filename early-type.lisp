;;;; this file exists so that `TYPE' will be type-bound in ir1.lisp

(uiop:define-package :hindley-milner/early-type
    (:nicknames :early-type)
  (:use :hindley-milner/defenum :cl)
  (:shadow :type)
  (:export
   :type
   :make-type-primitive :type-primitive :type-primitive-name :*boolean* :*fixnum*
   :-> :make--> :->-input :->-output))
(cl:in-package :hindley-milner/early-type)

(defenum type
    (symbol ; denoting a type-variable
     (type-primitive ((name t)))
     (-> ((input type)
          (output type)))))
