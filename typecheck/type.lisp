(uiop:define-package :hindley-milner/typecheck/type
    (:nicknames :type)
  (:mix :hindley-milner/subst :hindley-milner/defenum :trivial-types :cl)
  (:import-from :gefjon-utils)
  (:shadow :type)
  (:export

   :type
   :type-primitive :make-type-primitive :type-primitive-name :*boolean* :*fixnum* :*void*
   :-> :make--> :->-input :->-output
   
   :new-type-variable

   :type-scheme :make-type-scheme :type-scheme-bindings :type-scheme-body

   :type-env

   :lazy-type-mixin))
(cl:in-package :hindley-milner/typecheck/type)

(defenum type
    (symbol ; denoting a type-variable
     (type-primitive ((name t)))
     (-> ((input type)
          (output type))))
  ;; intentionally avoid auto-generating `SUBST-RECURSE' methods
  ;; because you shouldn't recurse into `TYPE-PRIMITIVE'
  :defstruct gefjon-utils:defstruct)

;; `->' still gets a `SUBST-RECURSE' method, tho
(define-subst ->
  (make-->
   (recurse (->-input ->))
   (recurse (->-output ->))))

(defvar *boolean* (make-type-primitive 'cl:boolean))
(defvar *fixnum* (make-type-primitive 'cl:fixnum))
(defvar *void* (make-type-primitive 'cl:null))

(defun new-type-variable (&optional (name "type-variable-"))
  (let ((name-string (etypecase name
                       (symbol (symbol-name name))
                       (string name))))
    (gensym name-string)))

(gefjon-utils:defstruct type-scheme
  ((bindings (proper-list symbol))
   (body type)))

(deftype type-env ()
  '(association-list symbol type-scheme))
