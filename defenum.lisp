(uiop:define-package :hindley-milner/defenum
  (:mix :hindley-milner/subst :cl)
  (:nicknames :enum)
  (:export :defenum))
(cl:in-package :hindley-milner/defenum)

(defmacro defenum (type-name variants &key (defstruct 'defstruct-with-subst))
  "define an enumeration aka a sum type.

TYPE-NAME should be a symbol, which will be `DEFTYPE'd to the new
type.

VARIANTS should be a list of variant-descriptors, each of which may be
either:
 - a list suitable to become the CDR of an invocation of DEFSTRUCT.
 - a symbol which names an already-defined type.

DEFSTRUCT should be a symbol which names a macro, to be invoked to
define the structures for individual variants.

this will define a struct (using the DEFSTRUCT key argument) for each
non-symbol variant, and define a type (using a `DEFTYPE' whose body is
an `OR') which contains all the variants.

for example:

  (DEFENUM my-enum
    ((variant-a ((slot-a fixnum)
                 (slot-b t)))
     (variant-b ((slot-foo symbol)))
     null))

this defines structs `VARIANT-A' and `VARIANT-B', and a type `MY-ENUM'
which may be an instance of either variant or of `NULL' (i.e. exactly
`NIL')."
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

