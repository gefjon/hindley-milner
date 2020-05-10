(uiop:define-package :hindley-milner/primop
  (:mix :hindley-milner/prologue :cl)
  (:import-from :hindley-milner/syntax/package)
  (:export
   :operator)
  (:nicknames :primop))
(cl:in-package :hindley-milner/primop)

(deftype operator ()
  '(member hm:+ hm:- hm:* hm:/ hm:=))
