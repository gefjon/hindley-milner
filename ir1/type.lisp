(uiop:define-package :hindley-milner/ir1/type
    (:mix :hindley-milner/subst :hindley-milner/prologue :trivial-types :cl)
  (:nicknames :ir1-type)
  (:import-from :gefjon-utils)
  (:shadow :type)
  (:export

   :type
   :type-variable :type-variable-name
   :type-primitive :type-primitive-name :*boolean* :*fixnum* :*void*
   :-> :->-input :->-output
   
   :new-type-variable

   :type-scheme :type-scheme-bindings :type-scheme-body

   :type-env :type-env-lookup))
(cl:in-package :hindley-milner/ir1/type)

(defenum type ()
  ((type-variable ((name symbol)))
   (type-primitive ((name t)))
   (-> ((input type)
        (output type)))))

;; note that `SUBST' does not recurse into `TYPE-PRIMITIVE', because
;; `TYPE-PRIMITIVE-NAME's should not be substituted

(subst:recurse-on-slots ->
  input output)

(defvar *boolean* (make-instance 'type-primitive :name 'cl:boolean))
(defvar *fixnum* (make-instance 'type-primitive :name 'cl:fixnum))
(defvar *void* (make-instance 'type-primitive :name 'cl:null))

(defun new-type-variable (&optional (name "type-variable-"))
  (let ((name-string (etypecase name
                       (type-variable (symbol-name (type-variable-name name)))
                       (symbol (symbol-name name))
                       (string name))))
    (make-instance 'type-variable
                   :name (gensym name-string))))

(gefjon-utils:defclass type-scheme
  ((bindings (proper-list type-variable))
   (body type))
  :superclasses (gefjon-utils:print-all-slots-mixin))

(subst:recurse-on-slots type-scheme
  bindings body)

(deftype type-env ()
  "maps term variables to their type schemes"
  '(association-list symbol type-scheme))

(declaim (ftype (function (type-env symbol) type-scheme)
                type-env-lookup))
(defun type-env-lookup (type-env symbol)
  (or (cdr (assoc symbol type-env))
      (error "symbol ~s unbound in type-env ~s" symbol type-env)))
