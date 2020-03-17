(uiop:define-package :hindley-milner/ir2/instr
    (:mix
     :hindley-milner/prologue
     :hindley-milner/ir2/place
     :hindley-milner/ir2/repr-type
     :cl)
  (:import-from :hindley-milner/syntax
   :operator)
  (:export
   :instr
   :const :const-type :const-value
   :get-var :get-var-src
   :set-var :set-var-dest
   :drop
   :binop :binop-op
   :label :label-name
   :goto :goto-target
   :goto-if :goto-if-target
   :call :call-procedure :call-function-type
   :ret :ret-type
   :func-pointer :func-pointer-name))
(cl:in-package :hindley-milner/ir2/instr)

(deftype label-name ()
  'symbol)

(defenum instr ()
  ((const ; pushes a value to the stack
          ((type repr-type)
           (value t)))
   (get-var ; pushes a value to the stack
            ((src place)))
   (set-var ; takes a value from the stack
            ((dest place)))
   (drop ; discards a value from the stack
    ())
   (binop ; takes two inputs from the stack, pushes a result
          ((op operator)))
   (label ((name label-name)))
   (goto ((target label-name)))
   (goto-if ; takes a boolean off the stack
            ((target label-name)))
   (call ; takes args followed by a function from the stack
         ((function-type function-type)))
   (ret ; takes a value from the stack, unless the type is :void
        ((type repr-type)))
   (func-pointer ; pushes a function pointer onto the stack
    ((name symbol)))))
