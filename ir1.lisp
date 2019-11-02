(uiop:define-package :hindley-milner/ir1
    (:mix :hindley-milner/defenum :trivial-types :cl)
  (:nicknames :ir1)
  (:import-from :hindley-milner/syntax
                :literal :clause)
  (:shadow :funcall :lambda :let :quote :if :binop :type :progn))
(cl:in-package :hindley-milner/ir1)

(deftype type-primitive ()
  '(member fixnum boolean))

(deftype type-variable ()
  '(and symbol (not type-primitive)))

(defenum type
  type-primitive
  type-variable
  (-> ((input type)
       (output type))))

(defenum expr
  symbol
  (quote ((it syntax:literal)))
  (funcall ((function expr)
            (arg expr)))
  (lambda ((binding symbol)
           (body expr)))
  (let ((binding symbol)
        (initform expr)
        (body expr)))
  (if ((predicate expr)
       (then-case expr)
       (else-case expr)))
  (binop ((op syntax:operator)
          (lhs expr)
          (rhs expr)))
  (progn ((side-effects (proper-list expr))
          (return-value expr))))

;; (funcall a b c) => (funcall (funcall a b) c)

(declaim (ftype (function (syntax:funcall) funcall) transform-funcall))
(defun transform-funcall (funcall)
  (reduce #'make-funcall (syntax:funcall-args funcall)
          :initial-value (syntax:funcall-function funcall)))

(declaim (ftype (function ((proper-list clause)) progn) transform-implicit-progn))
(defun transform-implicit-progn (progn)
  ;; an equivalent implementation is:
  ;;
  ;; (make-progn (subseq progn 0 (1- (length progn)))
  ;;             (car (last progn)))
  (let* ((backwards (reverse progn))
         (return-value (first backwards))
         (side-effects-backwards (rest backwards))
         (side-effects (nreverse side-effects-backwards)))
    (make-progn side-effects return-value)))

;; (lambda (a b) (c d)) => (lambda a (lambda b (progn c d)))

(declaim (ftype (function (syntax:lambda) lambda) transform-lambda))
(defun transform-lambda (lambda)
  (flet ((reduce-lambda (binding body)
           (make-lambda binding body)))
    (reduce #'reduce-lambda (syntax:lambda-bindings lambda)
            :from-end t
            :initial-value (transform-implicit-progn (syntax:lambda-body lambda)))))

;; (let ((a b) (c d)) e f) => (let a b (let c d (progn e f)))

(declaim (ftype (function (syntax:let) let) transform-let))
(defun transform-let (let)
  (flet ((reduce-let (definition body)
           (make-let (syntax:definition-binding definition)
                     (syntax:definition-value definition)
                     body)))
    (reduce #'reduce-let (syntax:let-bindings let)
            :from-end t
            :initial-value (transform-implicit-progn (syntax:let-body let)))))

(declaim (ftype (function (clause) expr) parse))
(defun parse (clause)
  (etypecase clause
    (symbol clause)
    (syntax:literal (make-quote clause))
    (syntax:funcall (transform-funcall clause))
    (syntax:lambda (transform-lambda clause))
    (syntax:let (transform-let clause))
    (syntax:if (make-if (syntax:if-predicate clause)
                        (syntax:if-then-case classe)
                        (syntax:if-else-case clause)))
    (syntax:binop (make-binop (syntax:binop-op clause)
                              (syntax:binop-lhs clause)
                              (syntax:binop-rhs clause)))))
