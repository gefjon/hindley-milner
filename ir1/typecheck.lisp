(uiop:define-package :hindley-milner/ir1/typecheck
  (:mix :cl)
  (:import-from :hindley-milner/ir1/typecheck/substitute
   :substitution :apply-substitution)
  (:import-from :hindley-milner/ir1/typecheck/infer
   :infer-program)
  (:import-from :hindley-milner/ir1/typecheck/unify
   :solve)
  (:export :infer-program-types :*top-level-type-env*))
(cl:in-package :hindley-milner/ir1/typecheck)

(defun infer-program-types (untyped-program)
  "returns a new `IR1:EXPR' that is like UNTYPED-PROGRAM, except its `TYPE' and `SCHEME' slots are populated with inferred types"
  (multiple-value-bind (inferred-program constraints)
      (infer-program untyped-program)
    (apply-substitution (solve constraints) inferred-program)))
