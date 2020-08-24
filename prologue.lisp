;;;; stuff that i expect to be useful in most or all files

(uiop:define-package :hindley-milner/prologue
  (:use :cl)
  (:mix-reexport
   :hindley-milner/prologue/typedec
   :hindley-milner/prologue/hash-set
   :hindley-milner/prologue/clos)
  (:nicknames :prologue)
  (:import-from :genhash
   :hashref)
  (:import-from :alexandria
   :with-gensyms :curry :rcurry :compose :symbolicate :make-gensym)
  (:import-from :trivial-types
   :tuple)
  (:export
   :adjustable-vector
   :specialized-vector
   :hash-map-of
   :ensure-get
   :define-special
   :ensure-find
   :format-gensym
   :define-primitive-types

   ;; alexandria reexports
   :curry :rcurry :with-gensyms :compose :make-gensym :symbolicate))
(cl:in-package :hindley-milner/prologue)

(defun vector-adjustable-p (vector)
  (and (vectorp vector)
       (adjustable-array-p vector)
       (array-has-fill-pointer-p vector)))

(deftype adjustable-vector (&optional element-type)
  `(and (vector ,element-type) (satisfies vector-adjustable-p)))

(defmacro adjustable-vector (type &rest initial-contents)
  `(make-adjustable-vector :element-type ,type
                           :initial-contents (list ,@initial-contents)))

(defmacro make-adjustable-vector (&key (element-type t) initial-contents)
  (with-gensyms (length contents)
    `(let* ((,contents ,initial-contents)
            (,length (length ,contents)))
       (make-array ,length
                   :element-type ',element-type
                   :initial-contents ,contents
                   :fill-pointer ,length
                   :adjustable t))))

(defmacro specialized-vector (type &rest contents)
  `(let ((contents (list ,@contents)))
     (make-array (length contents)
                 :element-type ',type
                 :initial-contents contents)))

(deftype hash-map-of (&optional key value)
  ;; Note that this doesn't expand to `cl:hash-table', as it wants to
  ;; support `genhash'. That package doesn't export a type-name for
  ;; its hash tables.
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

(defmacro define-primitive-types (prim-class &body names)
  (flet ((defprim (name)
           (check-type name symbol)
           `(defvar ,(symbolicate '* (symbol-name name) '*)
              (make-instance ',prim-class
                             :primitive ,name))))
    `(progn ,@(mapcar #'defprim names))))
