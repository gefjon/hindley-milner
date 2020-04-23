(uiop:define-package :hindley-milner/cps/expr
    (:mix
     :hindley-milner/prologue
     :cl)
    (:import-from :hindley-milner/primop
     :operator)
    (:import-from :alexandria
     :symbolicate)
    (:shadow
   :type :variable :let :if :apply :throw :prog2)
  (:export
   :type
   :variable :variable-name :variable-type

   :definition :definition-name
   :function-like-definition :function-like-definition-body :function-like-definition-closes-over
   :fdefn :fdefn-arglist :fdefn-continuation-arg
   :contdefn :contdefn-arg
   :constdefn :constdefn-value

   :expr
   :let :let-var :let-prim-op :let-args :let-in
   :bind :bind-defn :bind-in
   :if :if-predicate :if-then-clause :if-else-clause
   :apply :apply-func :apply-args :apply-continuation
   :throw :throw-cont :throw-arg))
(cl:in-package :hindley-milner/cps/expr)

(def-c-enum type
  :void :boolean :fixnum :function :continuation)

(define-class variable
    ((name symbol)
     (type type)))

(define-class definition
    ((name variable)))

(define-class function-like-definition
    ((body expr)
     (closes-over (vector variable) :may-init-unbound t))
  :superclasses (definition))

(define-class fdefn
    ((arglist (vector variable))
     (continuation-arg variable))
  :superclasses (function-like-definition))

(define-class contdefn
    ((arg variable))
  :superclasses (function-like-definition))

(define-class constdefn
    ((value t))
  :superclasses (definition))

(defenum expr ()
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
