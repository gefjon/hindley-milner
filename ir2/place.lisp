(uiop:define-package :hindley-milner/ir2/place
    (:mix :hindley-milner/ir2/repr-type :cl)
  (:shadowing-import-from :gefjon-utils
   :defclass)
  (:export :place :place-name :place-type))
(cl:in-package :hindley-milner/ir2/place)

(defclass place
    ((name symbol)
     (type repr-type)))
