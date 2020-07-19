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
   :variable :let :if :apply)
  (:export
   :variable :type
   :local :name
   :closure :name :corresponding-local
   :constant :value
   
   :closure-vars

   :expr
   :let :var :prim-op :args :in
   :proc :name :body :closes-over :arglist :in
   :if :predicate :then-clause :else-clause
   :apply :func :args :continuation
   :throw :cont :arg))
(cl:in-package :hindley-milner/cps/expr)

(define-enum variable ((type repr-type))
  ((local ((name symbol)))
   (closure ((name symbol)
             (corresponding-local local)))
   (constant ((value t)))))

(deftype closure-vars ()
  '(adjustable-vector closure))

(define-enum expr ()
  ((let ((var variable)
         (prim-op operator)
         (args (vector variable))
         (in expr)))
   (proc ((name local)
          (body expr)
          (closes-over closure-vars :may-init-unbound t)
          (arglist (vector local))
          (in expr)))
   (if ((predicate variable)
        (then-clause expr)
        (else-clause expr)))
   (apply ((func variable)
           (args (vector variable))))))
