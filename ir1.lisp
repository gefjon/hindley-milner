(uiop:define-package :hindley-milner/ir1
  (:mix
   :hindley-milner/ir1/type
   :hindley-milner/ir1/expr
   :cl)
  (:import-from :hindley-milner/ir1/parse
   :parse-program)
  (:import-from :hindley-milner/ir1/monomorphize
   :monomorphize-program)
  (:import-from :hindley-milner/ir1/typecheck
   :infer-program-types)
  (:nicknames :ir1)
  (:reexport :hindley-milner/ir1/type :hindley-milner/ir1/expr)
  (:export :ir1-trans))
(in-package :hindley-milner/ir1)

(defun ir1-trans (surface-syntax)
  (monomorphize-program (infer-program-types (parse-program surface-syntax))))
