(uiop:define-package :hindley-milner/compile
    (:nicknames :compile)
  (:use :cl)
  (:shadow :compile)
  (:import-from :hindley-milner/syntax
   :read-program-from-file)
  (:import-from :hindley-milner/ir1
   :ir1-trans)
  (:import-from :hindley-milner/cps
   :cps-trans)
  (:import-from :hindley-milner/three-address
   :3adr-transform)
  (:import-from :hindley-milner/ir4
   :ir4-trans)
  (:import-from :hindley-milner/llvm-emit
   :emit-to-file)
  (:export :compile))
(cl:in-package :hindley-milner/compile)

(defun compile (infile outfile)
  (let* ((surface-syntax (read-program-from-file infile))
         (ir1 (ir1-trans surface-syntax))
         (cps (cps-trans ir1))
         (three-address (3adr-transform cps))
         (ir4 (ir4-trans three-address)))
    (emit-to-file outfile ir4)))
