(uiop:define-package :hindley-milner/cps
  (:nicknames :cps)
  (:mix
   :hindley-milner/cps/type
   :hindley-milner/cps/expr
   :hindley-milner/cps/trans
   :hindley-milner/cps/closure
   :cl)
  (:reexport
   :hindley-milner/cps/type
   :hindley-milner/cps/expr)
  (:export :cps-trans))
(cl:in-package :hindley-milner/cps)

(defun cps-trans (ir1-program)
  (make-closures-explicit (cps-transform ir1-program)))
