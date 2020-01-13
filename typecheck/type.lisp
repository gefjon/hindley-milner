(uiop:define-package :hindley-milner/typecheck/type
    (:nicknames :type)
  (:mix :hindley-milner/subst :hindley-milner/defenum :trivial-types :cl)
  (:import-from :gefjon-utils)
  (:import-from :hindley-milner/ir1)
  (:shadow :type)
  (:export

   :type
   :type-variable :type-variable-name
   :type-primitive :type-primitive-name :*boolean* :*fixnum* :*void*
   :-> :->-input :->-output
   
   :new-type-variable

   :type-scheme :type-scheme-bindings :type-scheme-body

   :type-env :type-env-lookup))
(cl:in-package :hindley-milner/typecheck/type)

(defenum type ()
  ((type-variable ((name symbol)))
   (type-primitive ((name t)))
   (-> ((input type)
        (output type)))))

;; note that `SUBST' does not recurse into `TYPE-PRIMITIVE' or
;; `TYPE-VARIABLE', to preserve `EQ' identity on those types

(define-subst ->
  (make-instance '->
   :input (recurse (->-input ->))
   :output (recurse (->-output ->))))

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
   (body type)))

(deftype type-env ()
  "maps term variables to their type schemes"
  '(association-list symbol type-scheme))

(declaim (ftype (function (type-env symbol) type-scheme)
                type-env-lookup))
(defun type-env-lookup (type-env symbol)
  (or (cdr (assoc symbol type-env))
      (error "symbol ~s unbound in type-env ~s" symbol type-env)))
