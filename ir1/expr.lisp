(uiop:define-package :hindley-milner/ir1/expr
  (:mix
   :hindley-milner/prologue
   :cl)
  (:import-from :hindley-milner/subst);; for `RECURSE-ON-SLOTS'
  (:import-from :hindley-milner/primop
   :operator)
  (:shadow :variable :quote :funcall :lambda :let :if :prog2)
  (:export

   :definition :definition-name :definition-initform
   :untyped
   :polymorphic :polymorphic-scheme
   :monomorphic :monomorphic-type
   
   :expr :expr-type
   :variable :variable-name
   :quote :quote-it
   :funcall :funcall-function :funcall-args
   :lambda  :lambda-bindings :lambda-body
   :let :let-def :let-body
   :if :if-predicate :if-then-case :if-else-case
   :primop :primop-op :primop-args
   :prog2 :prog2-side-effect :prog2-return-value

   :program :program-definitions :program-entry))
(cl:in-package :hindley-milner/ir1/expr)

(defenum definition ((name symbol)
                     (initform expr))
         ((untyped ())
          (polymorphic ((scheme type-scheme)))
          (monomorphic ((type type)))))

(subst:recurse-on-slots untyped
  name initform)
(subst:recurse-on-slots polymorphic
  name initform scheme)
(subst:recurse-on-slots monomorphic
  name initform type)

(defenum expr ((type type :may-init-unbound t))
    ((variable ((name symbol)))
     (quote ((it t)))
     (funcall ((function expr)
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
             (return-value expr)))))

;;;; transformations from surface-syntax to ir1:
;; - implicit progns (e.g. in let or lambda) are made explicit

(subst:recurse-on-slots expr
  type)
;; this method is superseded by those below, applying only to `QUOTE'
;; and `VARIABLE', so each `RECURSE-ON-SLOTS' below must also list
;; `TYPE'
(subst:recurse-on-slots funcall
  type function args)
(subst:recurse-on-slots lambda
  type bindings body)
(subst:recurse-on-slots let
  type def body)
(subst:recurse-on-slots if
  type predicate then-case else-case)
(subst:recurse-on-slots primop
  type args)
(subst:recurse-on-slots prog2
  type side-effect return-value)

(define-class program
    ((definitions (vector definition))
     (entry expr)))

(subst:recurse-on-slots program
  definitions entry)
