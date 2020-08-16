(uiop:define-package :hindley-milner/ir1/expr
  (:mix
   :hindley-milner/ir1/type
   :hindley-milner/prologue
   :cl)
  (:import-from :hindley-milner/subst
   :subst-all-slots :subst-atom)
  (:import-from :hindley-milner/primop
   :operator)
  (:shadow :variable :quote :funcall :lambda :let :if :prog2)
  (:export

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

   :program :definitions :entry))
(cl:in-package :hindley-milner/ir1/expr)

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
           (return-value expr))))
  :superclasses (subst-all-slots))

(define-class program
    ((definitions (vector definition))
     (entry expr))
  :superclasses (subst-all-slots))
