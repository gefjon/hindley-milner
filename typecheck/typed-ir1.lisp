;;;; ir1 but with type annotations
(uiop:define-package :hindley-milner/typecheck/typed-ir1
    (:mix :hindley-milner/subst :hindley-milner/defenum :trivial-types :cl)
  (:nicknames :typed-ir1)
  (:shadowing-import-from :hindley-milner/typecheck/type
                          :type :type-scheme :-> :type-primitive)
  (:shadow :funcall :lambda :let :quote :if :binop :prog2 :variable)
  (:export

   :expr
   :variable :make-variable :variable-type :variable-name
   :quote :make-quote :quote-type :quote-it
   :funcall :make-funcall :funcall-type :funcall-function :funcall-arg
   :lambda :make-lambda :lambda-type :lambda-binding :lambda-body
   :let :make-let :let-type :let-binding :let-scheme :let-initform :let-body
   :if :make-if :if-type :if-predicate :if-then-case :if-else-case
   :binop :make-binop :binop-type :binop-op :binop-lhs :binop-rhs
   :prog2 :make-prog2 :prog2-type :prog2-side-effect :prog2-return-value

   :expr-type))
(cl:in-package :hindley-milner/typecheck/typed-ir1)

(defenum expr
    ;; each of these nodes has a slot `TYPE', which holds the node's return type when evaluated
    ((variable ((type type)
                (name symbol)))
     (quote ((type type-primitive)
             (it syntax:literal)))
     (funcall ((type type)
               (function expr)
               (arg expr)))
     (lambda ((type ->)
              (binding symbol)
              (body expr)))
     (let ((type type)
           (binding symbol)
           (scheme
             ;; prior to monomorphization, `SCHEME' will hold a
             ;; `TYPE-SCHEME'. after the monomorphization pass, it
             ;; will hold a `TYPE' instead. at no point during
             ;; compilation should nodes with `TYPE-SCHEME's coexist
             ;; with nodes with `TYPE's in this slot.
             (or type-scheme type))
           (initform expr)
           (body expr)))
     (if ((type type)
          (predicate expr)
          (then-case expr)
          (else-case expr)))
     (binop ((type type)
             (op syntax:operator)
             (lhs expr)
             (rhs expr)))
     (prog2 ((type type)
             (side-effect expr)
             (return-value expr)))))

(defun expr-type (expr)
  (slot-value expr 'type))
