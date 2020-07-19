(uiop:define-package :hindley-milner/compile
    (:nicknames :compile)
  (:use :cl)
  (:shadow :compile)
  (:import-from :hindley-milner/typecheck
   :infer-program-types)
  (:import-from :hindley-milner/syntax
   :read-program-from-file)
  (:import-from :hindley-milner/ir1
   :parse-program)
  (:import-from :hindley-milner/monomorphize
   :monomorphize-program)
  (:import-from :hindley-milner/cps
   :cps-trans)
  (:import-from :hindley-milner/three-address
   :3adr-transform)
  (:export :compile))
(cl:in-package :hindley-milner/compile)

(defun compile (filename)
  (let* ((surface-syntax (read-program-from-file filename))
         (ir1 (parse-program surface-syntax))
         (typed (infer-program-types ir1))
         (monomorphic (monomorphize-program typed))
         (cps (cps-trans monomorphic))
         (three-address (3adr-transform cps)))
    three-address))
