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
   :func :variable :let :if :apply :prog2)
  (:export
   :variable :name :type
   :global :local :closure

   :closure-env-map

   :definition :name
   :procedure :body :closes-over :arglist
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

(deftype closure-env-map ()
  "A vector of conses which map variables from the enclosing scope to closure vars."
  '(adjustable-vector (cons variable closure)))

(define-enum definition ((name variable))
  ((procedure ((body expr)
               (closes-over closure-env-map :may-init-unbound t) ;; added in `closure.lisp'
               (arglist (vector variable))))
   (constant ((value t)))))

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
           (args (vector variable))))))
