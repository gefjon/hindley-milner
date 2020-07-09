;;;; stuff that i expect to be useful in most or all files

(uiop:define-package :hindley-milner/prologue
  (:use :cl)
  (:nicknames :prologue)
  (:import-from :gefjon-utils
   :symbol-concatenate
   :define-class
   :adjustable-vector :make-adjustable-vector :specialized-vector
   :shallow-copy :map-slots
   :|:| :-> :void :optional)
  (:import-from :genhash
   :hashref)
  (:import-from :alexandria
   :remove-from-plist)
  (:import-from :trivial-types
   :tuple)
  (:export
   :define-enum
   :extend-enum
   :hash-map-of
   :ensure-get
   :define-special
   :ensure-find
   :format-gensym

   ;; reexports from gefjon-utils
   :define-class
   :adjustable-vector :make-adjustable-vector :specialized-vector
   :shallow-copy :map-slots
   :|:| :-> :void :optional))
(cl:in-package :hindley-milner/prologue)

(defmacro define-enum (type-name common-slots variants &rest define-class-options)
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
  `(progn (define-class ,type-name ,common-slots ,@define-class-options)
          (extend-enum ,type-name ,variants)))

(defmacro extend-enum (enum-name variants)
  "add additional VARIANTS to an already-defined enum ENUM-NAME.

this just defines subclasses of ENUM-NAME."
  (flet ((define-variant (variant)
           (destructuring-bind (variant-name unique-slots
                                &rest options &key superclasses &allow-other-keys)
               variant
             `(define-class ,variant-name
                ,unique-slots
                ;; put declared superclasses before the enum class, to
                ;; allow mixins on variants which supersede methods
                ;; from the enum class.
                :superclasses (,@superclasses ,enum-name)
                ,@(remove-from-plist options :superclasses)))))
    `(progn
       ,@(mapcar #'define-variant variants))))

(deftype hash-map-of (&optional key value)
  (declare (ignore key value))
  't)

(defmacro ensure-get (key map default)
  `(let* ((map ,map)
          (key ,key))
     (multiple-value-bind (value present-p) (hashref key map)
       (if present-p value
           (setf (hashref key map) ,default)))))

(defmacro define-special (name type &optional (docstring "special variable defined by `DEFINE-SPECIAL'"))
  "define a special variable NAME of TYPE which is globally unbound.

for example: (define-special *foo* fixnum)"
  `(progn
     (|:| ,name ,type)
     (defvar ,name)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (documentation ',name 'cl:variable) ,docstring))))

(defmacro ensure-find (search-for vector default &rest keys &key &allow-other-keys)
  `(let* ((vector ,vector)
          (found (find ,search-for vector ,@keys)))
     (or found
         (let* ((default ,default))
           (vector-push-extend default vector)
           default))))

(|:| #'format-gensym (-> (string &rest t) symbol))
(defun format-gensym (fmt &rest args)
  (gensym (apply #'format nil fmt args)))
