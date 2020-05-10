(uiop:define-package :hindley-milner/cps/expr
  (:mix
   :hindley-milner/cps/type
   :hindley-milner/prologue
   :cl)
  (:import-from :hindley-milner/primop
   :operator)
  (:import-from :alexandria
   :symbolicate)
  (:shadow
   :function :type :variable :let :if :apply :throw :prog2)
  (:export
   :variable :variable-name :variable-type
   :global :local :closure

   :closure-env

   :definition :definition-name
   :procedure :procedure-body :procedure-closes-over
   :function :function-arglist :function-continuation-arg
   :continuation :continuation-arg
   :constant :constant-value

   :expr
   :let :let-var :let-prim-op :let-args :let-in
   :bind :bind-defn :bind-in
   :if :if-predicate :if-then-clause :if-else-clause
   :apply :apply-func :apply-args :apply-continuation
   :throw :throw-cont :throw-arg))
(cl:in-package :hindley-milner/cps/expr)

(define-enum variable ((name symbol)
                       (type type))
  ((global ())
   (local ())
   (closure ())))

(define-class definition
    ((name variable)))

(deftype closure-env ()
  '(hash-map-of local closure))

(define-class procedure
    ((body expr)
     (closes-over closure-env :may-init-unbound t))
  :superclasses (definition))

(define-class function
    ((arglist (vector variable))
     (continuation-arg variable))
  :superclasses (procedure))

(define-class continuation
    ((arg variable))
  :superclasses (procedure))

(define-class constant
    ((value t))
  :superclasses (definition))

(define-enum expr ()
  ((let ((var variable)
         (prim-op operator)
         (args (vector variable))
         (in expr)))
   (bind ((defn definition)
          (in expr)))
   (if ((predicate variable)
        (then-clause expr)
        (else-clause expr)))
   (apply ((func variable)
           (args (vector variable))
           (continuation variable)))
   (throw ((cont variable)
           (arg variable)))))
