(uiop:define-package :hindley-milner/syntax
  (:nicknames :syntax)
  (:use :hindley-milner/defenum :trivial-types :cl)
  (:shadow :funcall :lambda :let :quote :if :binop)
  (:export

   :literal
   :operator

   :definition :make-definition :definition-binding :definition-value

   :clause
   :funcall :make-funcall :funcall-function :funcall-args
   :lambda :make-lambda :lambda-bindings :lambda-body
   :let :make-let :let-bindings :let-body
   :if :make-if :if-predicate :if-then-case :if-else-case
   :binop :make-binop :binop-op :binop-lhs :binop-rhs))
(cl:in-package :hindley-milner/syntax)

(defenum literal fixnum boolean)
(deftype operator ()
  '(member + - * / =))

(gefjon-utils:defstruct definition
  ((binding symbol)
   (value clause)))

(defenum clause
  symbol
  literal
  (funcall ((function clause)
            (args (proper-list clause))))
  (lambda ((bindings (proper-list symbol))
           (body (proper-list clause))))
  (let ((bindings (proper-list definition))
        (body (proper-list clause))))
  (if ((predicate clause)
       (then-case clause)
       (else-case clause)))
  (binop ((op operator)
          (lhs clause)
          (rhs clause))))
