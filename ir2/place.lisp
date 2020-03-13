(uiop:define-package :hindley-milner/ir2/place
    (:mix
     :hindley-milner/prologue
     :hindley-milner/ir2/repr-type
     :cl)
  (:shadowing-import-from :gefjon-utils
   :defclass)
  (:export
   :place :place-name :place-type
   :local :argument :global
   :place-variety))
(cl:in-package :hindley-milner/ir2/place)

(defenum place
    ((name symbol)
     (type repr-type))
  ((local ())
   (argument ())
   (global ())))

(deftype place-variety ()
  '(member local argument global))
