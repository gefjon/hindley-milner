(uiop:define-package :hindley-milner/cps/expr
    (:mix
     :hindley-milner/prologue
     :cl)
    (:import-from :hindley-milner/primop
     :operator)
  (:shadowing-import-from :gefjon-utils
   :defclass)
  (:shadow
   :type :variable :let :if :apply :throw :prog2)
  (:export
   :type :primitive
   :variable :variable-name :variable-type
   :expr
   :let :let-var :let-prim-op :let-args :let-in
   :const :const-name :const-value :const-in
   :cont :cont-name :cont-arg :cont-body :cont-in
   :func :func-name :func-arglist :func-continuation-arg :func-body :func-in
   :if :if-predicate :if-then-clause :if-else-clause
   :apply :apply-func :apply-args :apply-continuation
   :throw :throw-cont :throw-args))
(cl:in-package :hindley-milner/cps/expr)

(def-c-enum type
  :void :boolean :fixnum :function :continuation)

(defenum primitive ()
      ((binop ((op operator)))
       (const ((type type)
               (value t)))))

(defclass variable
    ((name symbol)
     (type type)))

(defenum expr ()
  ((let ((var variable)
         (prim-op primitive)
         (args (vector variable))
         (in expr)))
   (const ((name variable)
           (value t)
           (in expr)))
   (cont ((name variable)
          (arg variable)
          (body expr)
          (in expr)))
   (func ((name variable)
          (arglist (vector variable))
          (continuation-arg variable)
          (body expr)
          (in expr)))
   (if ((predicate variable)
        (then-clause expr)
        (else-clause expr)))
   (apply ((func variable)
           (args (vector variable))
           (continuation variable)))
   (throw ((cont variable)
           (arg variable)))))
