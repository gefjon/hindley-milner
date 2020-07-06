(uiop:define-package :hindley-milner/ir1/type
  (:mix :hindley-milner/subst :hindley-milner/prologue :cl)
  (:nicknames :ir1-type)
  (:import-from :trivial-types
   :proper-list :association-list)
  (:shadow :type)
  (:export

   :type
   :type-variable :name
   :type-primitive :name :*boolean* :*fixnum* :*void*
   :arrow :inputs :output
   
   :new-type-variable

   :type-scheme :bindings :body

   :type-env :type-env-lookup))
(cl:in-package :hindley-milner/ir1/type)

(define-enum type ()
  ((type-variable ((name symbol)))
   (type-primitive ((name t)))
   (arrow ((inputs (vector type))
           (output type))
          :superclasses (subst-all-slots))))

;; note that `SUBST' does not recurse into `TYPE-PRIMITIVE', because
;; `TYPE-PRIMITIVE-NAME's should not be substituted

(defvar *boolean* (make-instance 'type-primitive :name :boolean))
(defvar *fixnum* (make-instance 'type-primitive :name :fixnum))
(defvar *void* (make-instance 'type-primitive :name :void))

(defun new-type-variable (&optional (name "type-variable-"))
  (let ((name-string (etypecase name
                       (type-variable (symbol-name (name name)))
                       (symbol (symbol-name name))
                       (string name))))
    (make-instance 'type-variable
                   :name (gensym name-string))))

(define-class type-scheme
    ((bindings (proper-list type-variable))
     (body type))
  :superclasses (subst-all-slots))

(deftype type-env ()
  "maps term variables to their type schemes"
  '(association-list symbol type-scheme))

(declaim (ftype (function (type-env symbol) type-scheme)
                type-env-lookup))
(defun type-env-lookup (type-env symbol)
  (or (cdr (assoc symbol type-env))
      (error "symbol ~s unbound in type-env ~s" symbol type-env)))
