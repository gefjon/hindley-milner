(uiop:define-package :hindley-milner/syntax/clause
  (:mix :hindley-milner/prologue :cl)
  (:shadow :variable :quote :funcall :lambda :let :quote :if)
  (:import-from :hindley-milner/primop
   :operator)
  (:export

   :boolean-literal
   :literal

   :type-name

   :top-level-form
   :const :binding :value
   :struct :name :type-params :elts
   :enum :name :type-params :variants
   :fn :name :value

   :pattern
   :bind :name
   :exactly :value
   :ign
   :destruct :name :elts

   :clause
   :match :val :arms
   :variable :name
   :quote :it
   :funcall :func :args
   :lambda :bindings :body
   :let :bindings :body
   :if :predicate :then-case :else-case
   :primop :op :args

   :program :definitions :entry))
(cl:in-package :hindley-milner/syntax/clause)

(deftype boolean-literal ()
  "converted at parse-time into a `CL:BOOLEAN' i.e. either `T' or `NIL'"
  '(member hm:|true| hm:|false|))

;; booleans aren't really literals, because there's no way to get the
;; reader to produce one in a hindley-milner source file. this is in
;; contrast with other kinds of literals, which are produced by the
;; reader and unchanged by parsing.
(deftype literal () '(or fixnum boolean))

(deftype type-name () 'symbol)

(define-enum top-level-form ()
  ((const ((binding symbol)
           (value clause)))
   (struct ((name type-name)
            (type-params (vector type-name))
            (elts (vector type-name))))
   (enum ((name type-name)
          (type-params (vector type-name))
          (variants (vector struct))))
   (fn ((name symbol)
        (value lambda)))))

(define-enum pattern ()
  ((bind ((name symbol)))
   (exactly ((value literal)))
   (ign ())
   (destruct ((name symbol)
              (elts (vector pattern))))))

(define-enum clause ()
  ((match ((val clause)
           (arms (vector (cons pattern clause)))))
   (variable ((name symbol)))
   (quote ((it literal)))
   (funcall ((func clause)
             (args (vector clause))))
   (lambda ((bindings (vector symbol))
            (body (vector clause))))
   (let ((bindings (vector const))
         (body (vector clause))))
   (if ((predicate clause)
        (then-case clause)
        (else-case clause)))
   (primop ((op operator)
            (args (vector clause))))))

(define-class program
  ((definitions (vector top-level-form))
   (entry clause)))
