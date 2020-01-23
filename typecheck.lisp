(uiop:define-package :hindley-milner/typecheck
    (:nicknames :typecheck)
  (:use :cl)
  (:import-from :hindley-milner/typecheck/substitute
                :substitution :apply-substitution)
  (:import-from :hindley-milner/typecheck/infer)
  (:import-from :hindley-milner/typecheck/unify)
  (:export :infer-program-types :*top-level-type-env*))
(cl:in-package :hindley-milner/typecheck)

(defun infer-program-types (untyped-program)
  "returns a new `IR1:EXPR' that is like UNTYPED-PROGRAM, except its `TYPE' and `SCHEME' slots are populated with inferred types"
  (multiple-value-bind (inferred-program result-type constraints)
      (infer:infer untyped-program ())
    (declare (ignore result-type))
    (let ((substitution (unify:solve constraints)))
      (apply-substitution substitution inferred-program))))
