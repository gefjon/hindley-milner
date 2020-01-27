(uiop:define-package :hindley-milner/defenum
    (:use :cl)
  (:nicknames :enum)
  (:export :defenum :extend-enum))
(cl:in-package :hindley-milner/defenum)

(defmacro defenum (type-name common-slots variants)
  "define an enum or sum type named TYPE-NAME with COMMON-SLOTS.

this compiles into a superclass TYPE-NAME and a subtype for each of
the VARIANTS. it's likely incorrect to do (`MAKE-INSTANCE'
'TYPE-NAME), since TYPE-NAME is intended to be an abstract
superclass.

VARIANTS should be a list of variant-descriptors, each of which is a
list of the form (VARIANT-NAME UNIQUE-SLOTS).

UNIQUE-SLOTS is a list of slot-descriptors, each of which is a list of
the form (SLOT-NAME SLOT-TYPE `&KEY' INITFORM MAY-INIT-UNBOUND
ACCESSOR). the `&KEY' args all have sensible defaults."
  `(progn (gefjon-utils:defclass ,type-name ,common-slots
            :superclasses (gefjon-utils:print-all-slots-mixin))
          (extend-enum ,type-name ,variants)))

(defmacro extend-enum (enum-name variants)
  "add additional VARIANTS to an already-defined enum ENUM-NAME.

this just defines a bunch of superclasses of ENUM-NAME."
  (flet ((define-variant (variant)
           (destructuring-bind (variant-name unique-slots) variant
             `(gefjon-utils:defclass ,variant-name
                  ,unique-slots
                :superclasses (,enum-name)))))
    `(progn
       ,@(mapcar #'define-variant variants))))
