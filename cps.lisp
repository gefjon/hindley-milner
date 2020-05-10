(uiop:define-package :hindley-milner/cps
    (:nicknames :cps)
    (:use-reexport
     :hindley-milner/cps/type
     :hindley-milner/cps/expr
     :hindley-milner/cps/trans))
