(uiop:define-package :hindley-milner/ir4
  (:mix
   :hindley-milner/prologue
   :hindley-milner/ir4/expr
   :hindley-milner/ir4/trans
   :cl)
  (:import-from :hindley-milner/three-address ;; for `3adr:program'
                )
  (:reexport :hindley-milner/ir4/expr)
  (:export :ir4-trans))
(in-package :hindley-milner/ir4)

(|:| #'ir4-trans (-> (3adr:program) program))
(defun ir4-trans (3adr)
  (ir4-transform 3adr))
