(uiop:define-package :hindley-milner/primop
  (:mix :hindley-milner/prologue :cl)
  (:import-from :hindley-milner/syntax/package)
  (:shadowing-import-from :gefjon-utils
   :defclass)
  (:export
   :operator
   :make-closure-env :access-closure-env)
  (:nicknames :primop))
(cl:in-package :hindley-milner/primop)

(deftype operator ()
  '(member

    hm:+ hm:- hm:* hm:/ hm:=

    ;; these aren't intended to be read from surface syntax, but
    ;; rather will be inserted down the line
    make-closure-env access-closure-env))
