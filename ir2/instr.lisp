(uiop:define-package :hindley-milner/ir2/instr
    (:mix
     :hindley-milner/prologue
     :hindley-milner/ir2/place
     :hindley-milner/ir2/repr-type
     :cl)
  (:export
   :instr
   :const :const-dest :const-value
   :mov :mov-dest :mov-src
   :binop :binop-dest :binop-lhs :binop-rhs :binop-op
   :label :label-name
   :goto :goto-target
   :param :param-src
   :call :call-dest :call-procedure :call-param-count
   :ret :ret-val
   :func-pointer :func-pointer-dest :func-pointer-func))
(cl:in-package :hindley-milner/ir2/instr)

(deftype label-name ()
  'symbol)

(defenum instr ()
  ((const ((dest place)
           (value t)))
   (mov ((dest place)
         (src place)))
   (binop ((dest place)
           (lhs place)
           (rhs place)
           (op operator)))
   (label ((name label-name)))
   (goto ((target label-name)))
   (goto-if ((target label-name)
             (predicate place)))
   (param ((src place)))
   (call ((dest place)
          (procedure place)
          (param-count unsigned-byte)))
   (ret ((val place)))
   (func-pointer ((dest place)
                  (func symbol)))))
