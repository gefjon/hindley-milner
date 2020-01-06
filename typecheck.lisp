(uiop:define-package :hindley-milner/typecheck
    (:nicknames :typecheck)
  (:use :cl)
  (:import-from :hindley-milner/typecheck/substitute
                :substitution :apply-substitution)
  (:import-from :hindley-milner/typecheck/infer)
  (:import-from :hindley-milner/typecheck/unify)
  (:export :infer-program-types :*top-level-type-env*))
(cl:in-package :hindley-milner/typecheck)

(defun infer-program-types (ir1-program)
  (multiple-value-bind (inferred-program result-type constraints)
      (infer:infer ir1-program ())
    (declare (ignore result-type))
    (let ((substitution (unify:solve constraints)))
      (apply-substitution substitution inferred-program))))
