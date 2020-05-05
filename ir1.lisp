(uiop:define-package :hindley-milner/ir1
  (:mix
   :hindley-milner/ir1/type
   :hindley-milner/ir1/expr
   :hindley-milner/ir1/parse
   :cl)
  (:nicknames :ir1)
  (:reexport :hindley-milner/ir1/type :hindley-milner/ir1/expr :hindley-milner/ir1/parse))
(cl:in-package :hindley-milner/ir1)
