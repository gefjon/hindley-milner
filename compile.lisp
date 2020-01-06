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
  (:export :compile))
(cl:in-package :hindley-milner/compile)

(defun compile (filename)
  (let* ((surface-syntax (read-program-from-file filename))
         (ir1 (parse-program surface-syntax)))
    (infer-program-types ir1)))