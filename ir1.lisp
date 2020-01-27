(uiop:define-package :hindley-milner/ir1
    (:mix :hindley-milner/ir1/type :hindley-milner/defenum :trivial-types :cl)
  (:nicknames :ir1)
  (:import-from :hindley-milner/subst) ;; for `RECURSE-ON-SLOTS'
  (:import-from :hindley-milner/syntax
                :literal :clause)
  (:shadow :funcall :lambda :quote :if :binop :prog2 :variable)
  (:reexport :hindley-milner/ir1/type)
  (:export
   :expr :expr-type
   :variable :variable-name
   :quote :quote-it
   :funcall :funcall-function :funcall-arg
   :lambda  :lambda-binding :lambda-body
   :poly-let :poly-let-binding :poly-let-scheme :poly-let-initform :poly-let-body
   :if :if-predicate :if-then-case :if-else-case
   :binop :binop-op :binop-lhs :binop-rhs
   :prog2 :prog2-side-effect :prog2-return-value

   :parse :parse-program))
(cl:in-package :hindley-milner/ir1)

;;;; transformations from surface-syntax to ir1:
;; - functions are explicitly curried, so we transform lambdas and
;;   funcalls of multiple arguments into nested lambdas or
;;   funcalls. this makes type-checking cleaner, because every binding
;;   form introduces exactly one binding.
;;
;; - implicit progns (e.g. in let or lambda) are made explicit
;; 
;; - literals are tagged with a quote

(defenum expr ((type type :may-init-unbound t))
    ((variable ((name symbol)))
     (quote ((it syntax:literal)))
     (funcall ((function expr)
               (arg expr)))
     (lambda ((binding symbol)
              (body expr)))
     (poly-let ((binding symbol)
                (scheme type-scheme :may-init-unbound t)
                (initform expr)
                (body expr)))
     (if ((predicate expr)
          (then-case expr)
          (else-case expr)))
     (binop ((op syntax:operator)
             (lhs expr)
             (rhs expr)))
     (prog2 ((side-effect expr)
             (return-value expr)))))

(subst:recurse-on-slots expr
  type)
;; this method is superseded by those below, so each
;; `RECURSE-ON-SLOTS' below must also list `TYPE'
(subst:recurse-on-slots funcall
  type function arg)
(subst:recurse-on-slots lambda
  type binding body)
(subst:recurse-on-slots poly-let
  type scheme initform body)
(subst:recurse-on-slots if
  type predicate then-case else-case)
(subst:recurse-on-slots binop
  type lhs rhs)
(subst:recurse-on-slots prog2
  type side-effect return-value)

(defgeneric parse (clause)
  (:documentation "transform a `SYNTAX:CLAUSE' into an `IR1:CLAUSE'"))

(declaim (ftype (function ((proper-list clause)) (values expr &optional))
                transform-implicit-progn))
(defun transform-implicit-progn (progn)
  (cond ((null progn) (error "empty implicit progn"))
        ;; if there's only one clause, you can skip consing the progn
        ((null (rest progn)) (parse (first progn)))
        (:otherwise
         ;; an equivalent implementation is:
         ;;
         ;; (make-progn (subseq progn 0 (1- (length progn)))
         ;;             (car (last progn)))
         (flet ((reduce-prog2 (side-effect return-value)
                  (make-instance 'prog2
                                 :side-effect side-effect
                                 :return-value return-value)))
           (reduce #'reduce-prog2 progn :key #'parse)))))

(defmethod parse ((funcall syntax:funcall))
  "transform (funcall a b c) into (funcall (funcall a b) c)

TODO: handle the edge case of invoking a function on no arguments by transforming into an invocation of one empty argument"
  (flet ((reduce-funcall (function arg)
           (make-instance 'funcall
                          :function function
                          :arg arg)))
    (reduce #'reduce-funcall (mapcar #'parse (syntax:funcall-args funcall))
            :initial-value (parse (syntax:funcall-function funcall)))))

(defmethod parse ((lambda syntax:lambda))
  "transform (lambda (a b) (c d)) into (lambda a (lambda b (progn c d)))

TODO: correctly handle the edge case where (lambda-bindings lambda) is nil by transforming into a function of one empty argument"
  (flet ((reduce-lambda (binding body)
           (make-instance 'lambda
                          :binding binding
                          :body body)))
    (reduce #'reduce-lambda (syntax:lambda-bindings lambda)
            :from-end t
            :initial-value (transform-implicit-progn (syntax:lambda-body lambda)))))

(declaim (ftype (function (syntax:definition expr) (values expr &optional))
                let-from-definition))
(defun let-from-definition (definition body)
  "build an `IR1:POLY-LET' from a `SYNTAX:DEFINITION' and an `IR1:EXPR'

passed to `REDUCE' in parsers for `LET' and `SYNTAX:PROGRAM'"
  (make-instance 'poly-let
                 :binding (syntax:definition-binding definition)
                 :initform (parse (syntax:definition-value definition))
                 :body body))

(defmethod parse ((let syntax:let))
  "transform (let ((a b) (c d)) e f) into (let a b (let c d (progn e f)))

edge case: parses (let () a b) into (progn a b)"
  (reduce #'let-from-definition (syntax:let-bindings let)
          :from-end t
          :initial-value (transform-implicit-progn (syntax:let-body let))))

(defmethod parse ((if syntax:if))
  "boring. recurse on `PARSE'"
  (make-instance 'if
                 :predicate (parse (syntax:if-predicate if))
                 :then-case (parse (syntax:if-then-case if))
                 :else-case (parse (syntax:if-else-case if))))

(defmethod parse ((binop syntax:binop))
  "boring. recurse on `PARSE'"
  (make-instance 'binop
                 :op (parse (syntax:binop-op binop))
                 :lhs (parse (syntax:binop-lhs binop))
                 :rhs (parse (syntax:binop-rhs binop))))

(defmethod parse ((variable syntax:variable))
  (make-instance 'variable
                 :name (syntax:variable-name variable)))

(defmethod parse ((quote syntax:quote))
  (make-instance 'quote
                 :it (syntax:quote-it quote)))

(declaim (ftype (function (syntax:program) expr)
                parse-program))
(defun parse-program (program)
  "transform a `SYNTAX:PROGRAM' into an `IR1:EXPR'"
  (reduce #'let-from-definition (syntax:program-definitions program)
          :from-end t
          :initial-value (parse (syntax:program-entry program))))
