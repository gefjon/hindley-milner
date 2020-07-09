(uiop:define-package :hindley-milner/cps/expr
  (:mix
   :hindley-milner/repr-type
   :hindley-milner/prologue
   :cl)
  (:import-from :hindley-milner/primop
   :operator)
  (:import-from :alexandria
   :symbolicate)
  (:shadow
   :func :variable :let :if :apply :throw :prog2)
  (:export
   :variable :name :type
   :global :local :closure

   :closure-env-map

   :definition :name
   :procedure :body :closes-over
   :func :arglist :continuation-arg
   :continuation :arg
   :constant :value

   :expr
   :let :var :prim-op :args :in
   :bind :defn :in
   :if :predicate :then-clause :else-clause
   :apply :func :args :continuation
   :throw :cont :arg))
(cl:in-package :hindley-milner/cps/expr)

(define-enum variable ((name symbol)
                       (type type))
  ((global ())
   (local ())
   (closure ())))

(define-class definition
    ((name variable)))

(deftype closure-env-map ()
  "A vector of conses which map variables from the enclosing scope to closure vars."
  '(adjustable-vector (cons variable closure)))

(define-class procedure
    ((body expr)
     (closes-over closure-env-map :may-init-unbound t))
  :superclasses (definition))

(define-class func
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
