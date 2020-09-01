(uiop:define-package :hindley-milner/ir1/expr
  (:mix
   :hindley-milner/prologue
   :hindley-milner/subst
   :cl)
  (:shadow :type)
  (:import-from :hindley-milner/subst
   :subst-all-slots :subst-atom)
  (:import-from :hindley-milner/primop
   :operator)
  (:import-from :trivial-types
   :proper-list :association-list)
  (:shadow :variable :quote :funcall :lambda :let :if :prog2 :and)
  (:export

   :type
   :type-variable :name
   :type-primitive :name
   :arrow :inputs :output
   :struct :name :elts
   :enum :name :variants

   :*boolean* :*fixnum* :*void*

   :new-type-variable

   :type-scheme :bindings :body

   :type-env :type-env-lookup

   :definition :name :initform
   :untyped
   :polymorphic :scheme
   :monomorphic :type
   
   :expr :type
   :variable :variable-name
   :quote :it
   :funcall :func :args
   :lambda  :bindings :body
   :let :def :body
   :if :predicate :then-case :else-case
   :primop :op :args
   :prog2 :side-effect :return-value
   :discriminant-p :value :variant
   :assert-variant :src :constructor
   :match-exhausted
   :field-value :idx :aggregate :constructor
   :and :lhs :rhs

   :*true* :*false*
   
   :program :types :definitions :entry))
(cl:in-package :hindley-milner/ir1/expr)

(define-enum type ()
  ((type-variable ((name symbol))
                  :superclasses (subst-atom))
   (type-primitive ((name t))
                   :superclasses (subst-atom))
   (arrow ((inputs (vector type))
           (output type)))
   (struct ((name symbol)
            (elts (vector type))))
   (enum ((name symbol)
          (variants (vector struct)))))
  :superclasses (subst-all-slots))

;; note that `SUBST' does not recurse into `TYPE-PRIMITIVE', because
;; `TYPE-PRIMITIVE-NAME's should not be substituted

(defvar *boolean* (make-instance 'type-primitive :name :boolean))
(defvar *fixnum* (make-instance 'type-primitive :name :fixnum))
(defvar *void* (make-instance 'type-primitive :name :void))

(defun new-type-variable (&optional (name "type-variable-"))
  (let* ((name-string (etypecase name
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

(|:| #'type-env-lookup (-> (type-env symbol) type-scheme))
(defun type-env-lookup (type-env symbol)
  (or (cdr (assoc symbol type-env))
      (error "symbol ~s unbound in type-env ~s" symbol type-env)))

(define-class definition
    ((name symbol)
     (initform expr)
     (scheme type-scheme :may-init-unbound t))
  :superclasses (subst-all-slots))

(define-enum expr ((type type :may-init-unbound t))
  (;; `variable' and `quote' must subclass `subst-atom' so
   ;; that their slots are not substituted; this overwrites
   ;; the method from `expr' subclassing `subst-all-slots'.
   (variable ((name symbol))
             :superclasses (subst-atom))
   (quote ((it t))
          :superclasses (subst-atom))
   (funcall ((func expr)
             (args (vector expr))))
   (lambda ((bindings (vector symbol))
            (body expr)))
   (let ((def definition)
         (body expr)))
   (if ((predicate expr)
        (then-case expr)
        (else-case expr)))
   (primop ((op operator)
            (args (vector expr))))
   (prog2 ((side-effect expr)
           (return-value expr)))
   (discriminant-p ((value expr)
                    (variant symbol)))
   (assert-variant ((src expr)
                    (constructor symbol)))
   (match-exhausted ())
   (field-value ((idx (cl:and unsigned-byte fixnum))
                 (constructor symbol)
                 (aggregate expr)))
   (and ((lhs expr)
         (rhs expr))))
  :superclasses (subst-all-slots))

(defvar *true* (make-instance 'quote :it t))
(defvar *false* (make-instance 'quote :it nil))

(defmethod subst-recurse (new old (tree discriminant-p) test)
  (shallow-copy tree
                :value (subst new old (value tree) :test test)))

(defmethod subst-recurse (new old (tree assert-variant) test)
  (shallow-copy tree
                :src (subst new old (src tree) :test test)))

(defmethod subst-recurse (new old (tree field-value) test)
  (shallow-copy tree
                :aggregate (subst new old (aggregate tree) :test test)))

(define-class program
    ((types (vector type-scheme))
     (definitions (vector definition))
     (entry expr))
  :superclasses (subst-all-slots))
