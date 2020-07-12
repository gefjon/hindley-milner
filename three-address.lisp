(uiop:define-package :hindley-milner/three-address
  (:mix
   :hindley-milner/prologue
   :cl)
  (:mix-reexport
   :hindley-milner/three-address/expr
   :hindley-milner/three-address/trans
   :hindley-milner/three-address/liveness)
  (:import-from :hindley-milner/cps :expr)
  (:export
   :3adr-transform))
(in-package :hindley-milner/three-address)

(|:| #'3adr-transform (-> (expr) program))
(defun 3adr-transform (cps-program)
  (liveness-annotate (three-address-transform-program cps-program)))
