(uiop:define-package :hindley-milner/syntax/clause
  (:mix :hindley-milner/prologue :cl)
  (:shadow :variable :quote :funcall :lambda :let :quote :if)
  (:import-from :hindley-milner/primop
   :operator)
  (:export

   :boolean-literal
   :literal
   :definition :binding :value

   :clause
   :variable :name
   :quote :it
   :funcall :func :args
   :lambda :bindings :body
   :let :bindings :body
   :if :predicate :then-case :else-case
   :primop :op :args))
(cl:in-package :hindley-milner/syntax/clause)

(deftype boolean-literal ()
  "converted at parse-time into a `CL:BOOLEAN' i.e. either `T' or `NIL'"
  '(member hm:|true| hm:|false|))

;; booleans aren't really literals, because there's no way to get the
;; reader to produce one in a hindley-milner source file. this is in
;; contrast with other kinds of literals, which are produced by the
;; reader and unchanged by parsing.
(deftype literal () '(or fixnum boolean))

(define-class definition
    ((binding symbol)
     (value clause)))

(define-enum clause ()
  ((variable ((name symbol)))
   (quote ((it literal)))
   (funcall ((func clause)
             (args (vector clause))))
   (lambda ((bindings (vector symbol))
            (body (vector clause))))
   (let ((bindings (vector definition))
         (body (vector clause))))
   (if ((predicate clause)
        (then-case clause)
        (else-case clause)))
   (primop ((op operator)
            (args (vector clause))))))
