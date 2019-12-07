(uiop:define-package :hindley-milner/defenum
  (:use :cl)
  (:nicknames :enum)
  (:export :defenum))
(cl:in-package :hindley-milner/defenum)

(defmacro defenum (type-name variants &key (defstruct 'gefjon-utils:defstruct))
  (flet ((define-variant (variant)
           (etypecase variant
             (symbol nil)
             (list `(,defstruct ,@variant))))
         (type-name-for (variant)
           (etypecase variant
             (symbol variant)
             (list (first variant)))))
    `(progn
       ,@(remove nil (mapcar #'define-variant variants))
       (deftype ,type-name ()
         '(or ,@(mapcar #'type-name-for variants))))))

