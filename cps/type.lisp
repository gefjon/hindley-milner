(uiop:define-package :hindley-milner/cps/type
  (:mix
   :hindley-milner/prologue
   :cl)
  (:shadow :type)
  (:export :type))
(cl:in-package :hindley-milner/cps/type)

(define-c-enum type
  :void :boolean :fixnum :function :continuation)
