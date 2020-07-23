;;;; stuff that i expect to be useful in most or all files

(uiop:define-package :hindley-milner/prologue
  (:use :cl)
  (:nicknames :prologue)
  (:import-from :gefjon-utils
   :symbol-concatenate
   :define-class
   :adjustable-vector :make-adjustable-vector :specialized-vector
   :shallow-copy :map-slots :reduce-slots
   :|:| :-> :void :optional)
  (:import-from :genhash
   :hashref)
  (:import-from :alexandria
   :remove-from-plist :rcurry :symbolicate)
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
   :with-slot-accessors
   :hash-set
   :make-hash-set
   :hash-set-contains-p
   :hash-set-insert
   :hash-set-remove
   :hash-set-map
   :hash-set-count
   :hash-set-vector
   :standard-object-equalp
   :define-primitive-types

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

(defmacro with-slot-accessors (accessors instance &body body)
  "Like `with-accessors', but allows the shorthand from `with-slots' which binds the name of the accessor as the local."
  (flet ((canonicalize (accessor)
           (etypecase accessor
             ((cons symbol (cons symbol null)) accessor)
             (symbol `(,accessor ,accessor)))))
    `(with-accessors ,(mapcar #'canonicalize accessors) ,instance ,@body)))

(deftype hash-set (&optional eltype)
  `(hash-map-of ,eltype ,eltype))

(defun make-hash-set (&key (test #'eq))
  (make-hash-table :test test))

(|:| #'hash-set-contains-p (-> (t hash-set) (values t boolean)))
(defun hash-set-contains-p (elt set)
  "Test is SET contains ELT.

If present, return the version in the set as a primary value and `t' as a secondary value.
If not present, return `nil' as both primary and secondary values."
  (multiple-value-bind (entry foundp) (gethash elt set)
    (values
     (when foundp
       (assert (funcall (hash-table-test set) elt entry))
       entry)
     foundp)))

(|:| #'hash-set-insert (-> (t hash-set) void))
(defun hash-set-insert (elt set)
  "Insert ELT into SET.

Does no checking to see if SET already contains ELT."
  (setf (gethash elt set) elt))

(|:| #'hash-set-remove (-> (t hash-set) boolean))
(defun hash-set-remove (elt set)
  "Remove ELT from SET, returning `t' if it was present or `nil' if it was not."
  (remhash elt set))

(|:| #'hash-set-map (-> ((-> (t) void) hash-set) void))
(defun hash-set-map (func set)
  (flet ((maphash-func (key val)
           (assert (funcall (hash-table-test set) key val))
           (funcall func key)))
    (maphash #'maphash-func set))
  (values))

(|:| #'hash-set-count (-> (hash-set) unsigned-byte))
(defun hash-set-count (set)
  (hash-table-count set))

(|:| #'hash-set-vector (-> (hash-set) vector))
(defun hash-set-vector (set
                        &aux (vec (make-array (hash-set-count set)
                                              :adjustable t
                                              :fill-pointer 0)))
  (hash-set-map (rcurry #'vector-push vec) set)
  vec)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric standard-object-equalp (lhs rhs)
    (:method (lhs rhs)
      (equalp lhs rhs))
    (:method ((lhs standard-object) (rhs standard-object))
      (and (eq (class-of lhs) (class-of rhs))
           (reduce-slots #'(lambda (so-far lhs-val rhs-val)
                             (and so-far (standard-object-equalp lhs-val rhs-val)))
                         t
                         lhs rhs)))))

(defmacro define-primitive-types (prim-class &body names)
  (flet ((defprim (name)
           (check-type name symbol)
           `(defvar ,(symbolicate '* (symbol-name name) '*)
              (make-instance ',prim-class
                             :primitive ,name))))
    `(progn ,@(mapcar #'defprim names))))
