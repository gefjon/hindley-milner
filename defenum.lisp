(uiop:define-package :hindley-milner/defenum
  (:mix :hindley-milner/subst :cl)
  (:nicknames :enum)
  (:export :defenum))
(cl:in-package :hindley-milner/defenum)

(defmacro defenum (type-name common-slots variants)
  (flet ((define-variant (variant)
           (destructuring-bind (variant-name unique-slots) variant
             `(gefjon-utils:defclass ,variant-name
                  ,unique-slots
                :superclasses (,type-name)))))
    `(prog1
         (gefjon-utils:defclass ,type-name ,common-slots
           :superclasses (gefjon-utils:print-all-slots-mixin))
       ,@(mapcar #'define-variant variants))))

