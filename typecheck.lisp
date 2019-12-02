(uiop:define-package :hindley-milner/typecheck
    (:nicknames :typecheck)
  (:use :cl)
  (:import-from :hindley-milner/typecheck/infer)
  (:import-from :hindley-milner/typecheck/unify))
(cl:in-package :hindley-milner/typecheck)

