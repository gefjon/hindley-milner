(uiop:define-package :hindley-milner/repr-type
  (:mix
   :hindley-milner/prologue
   :cl)
  (:shadow :type)
  (:export :type))
(cl:in-package :hindley-milner/repr-type)

(define-c-enum type
  :void :boolean :fixnum :function :continuation)
