(uiop:define-package :hindley-milner/ir1
    (:mix :hindley-milner/defenum :trivial-types :cl)
  (:nicknames :ir1)
  (:import-from :hindley-milner/syntax
                :literal :clause)
  (:shadow :funcall :lambda :let :quote :if :binop :type :progn)
  (:export

   :expr
   :quote :make-quote :quote-it
   :funcall :make-funcall :funcall-function :funcall-arg
   :lambda :make-lambda :lambda-binding :lambda-body
   :let :make-let :let-binding :let-initform :let-body
   :if :make-if :if-predicate :if-then-case :if-else-case
   :binop :make-binop :binop-op :binop-lhs :binop-rhs
   :progn :make-progn :progn-side-effects :progn-return-value

   :parse))
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

;; TODO: handle the edge case of invoking a function on no arguments
(declaim (ftype (function (syntax:funcall) funcall) transform-funcall))
(defun transform-funcall (funcall)
  (reduce #'make-funcall (mapcar #'parse (syntax:funcall-args funcall))
          :initial-value (syntax:funcall-function funcall)))

(declaim (ftype (function ((proper-list clause)) expr) transform-implicit-progn))
(defun transform-implicit-progn (progn)
  (cond ((null progn) (error "empty implicit progn"))
        ;; if there's only one clause, you can skip consing the progn
        ((null (rest progn)) (parse (first progn)))
        (:otherwise
         ;; an equivalent implementation is:
         ;;
         ;; (make-progn (subseq progn 0 (1- (length progn)))
         ;;             (car (last progn)))

         (let* ((backwards (reverse progn))
                           (return-value (parse (first backwards)))
                           (side-effects-backwards (rest backwards))
                           (side-effects (nreverse (mapcar #'parse side-effects-backwards))))
                      (make-progn side-effects return-value)))))

;; (lambda (a b) (c d)) => (lambda a (lambda b (progn c d)))

;; TODO: correctly handle the edge case where (lambda-bindings lambda) is nil
(declaim (ftype (function (syntax:lambda) lambda) transform-lambda))
(defun transform-lambda (lambda)
  (flet ((reduce-lambda (binding body)
           (make-lambda binding body)))
    (reduce #'reduce-lambda (syntax:lambda-bindings lambda)
            :from-end t
            :initial-value (transform-implicit-progn (syntax:lambda-body lambda)))))

;; (let ((a b) (c d)) e f) => (let a b (let c d (progn e f)))

;; has declaimed return type EXPR (not the more specific LET) because
;; of an edge-case when parsing a let that binds no symbols
(declaim (ftype (function (syntax:let) expr) transform-let))
(defun transform-let (let)
  (flet ((reduce-let (definition body)
           (make-let (syntax:definition-binding definition)
                     (parse (syntax:definition-value definition))
                     body)))
    (reduce #'reduce-let (syntax:let-bindings let)
            :from-end t
            :initial-value (transform-implicit-progn (syntax:let-body let)))))

;; these last two transforms just recurse on PARSE 

(declaim (ftype (function (syntax:if) if) transform-if))
(defun transform-if (if)
  (make-if (parse (syntax:if-predicate if))
           (parse (syntax:if-then-case if))
           (parse (syntax:if-else-case if))))

(declaim (ftype (function (syntax:binop) binop) transform-binop))
(defun transform-binop (binop)
  (make-binop (parse (syntax:binop-op binop))
              (parse (syntax:binop-lhs binop))
              (parse (syntax:binop-rhs binop))))

;; and now, a big branch on the kind of clause. this could change into
;; a generic function, which would make the typecase implicit, but i
;; see no real reason to prefer that.

(declaim (ftype (function (clause) expr) parse))
(defun parse (clause)
  (etypecase clause
    (symbol clause)
    (syntax:literal (make-quote clause))
    (syntax:funcall (transform-funcall clause))
    (syntax:lambda (transform-lambda clause))
    (syntax:let (transform-let clause))
    (syntax:if (transform-if clause))
    (syntax:binop (transform-binop clause))))
