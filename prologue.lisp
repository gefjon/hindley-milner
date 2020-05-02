;;;; stuff that i expect to be useful in most or all files

(uiop:define-package :hindley-milner/prologue
  (:use :cl)
  (:nicknames :prologue)
  (:import-from :gefjon-utils
   :symbol-concatenate
   :define-class
   :adjustable-vector :make-adjustable-vector :specialized-vector
   :shallow-copy
   :|:| :->)
  (:import-from :trivial-types
   :tuple)
  (:import-from :generic-cl :hash-map)
  (:export
   :defenum
   :extend-enum
   :def-c-enum
   :hash-map-of

   ;; reexports from gefjon-utils
   :define-class :|:| :-> :make-adjustable-vector :adjustable-vector :specialized-vector :shallow-copy))
(cl:in-package :hindley-milner/prologue)

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
  `(progn (define-class ,type-name ,common-slots)
          (extend-enum ,type-name ,variants)))

(defmacro extend-enum (enum-name variants)
  "add additional VARIANTS to an already-defined enum ENUM-NAME.

this just defines subclasses of ENUM-NAME."
  (flet ((define-variant (variant)
           (destructuring-bind (variant-name unique-slots) variant
             `(define-class ,variant-name
                ,unique-slots
                :superclasses (,enum-name)))))
    `(progn
       ,@(mapcar #'define-variant variants))))

(defmacro def-c-enum (name &rest variants)
  "define a c-style enum, which associates several keywords with integer values.

each of the VARIANTS should be either:
- a keyword
- a list of (KEYWORD VALUE), where VALUE is an integer.

as in c, unsupplied values will increment by one from the last
supplied value, starting at 0 if no value is supplied."
  (let ((idx -1))
    (flet ((variant-name (variant)
             (etypecase variant
               (keyword variant)
               ((tuple keyword integer) (first variant))))
           (variant-case-clause (variant)
             (etypecase variant
               (keyword (list variant (incf idx)))
               ((tuple keyword integer)
                (setf idx (second variant))
                variant))))
      `(progn (deftype ,name ()
                '(member ,@(mapcar #'variant-name variants)))
              (defun ,(symbol-concatenate name '-to-int) (,name)
                (ecase ,name
                  ,@(mapcar #'variant-case-clause variants)))))))

(deftype hash-map-of (&optional key value)
  (declare (ignore key value))
  'hash-map)
