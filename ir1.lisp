(uiop:define-package :hindley-milner/ir1
  (:mix
   :hindley-milner/ir1/type
   :hindley-milner/prologue
   :iterate
   :cl)
  (:import-from :uiop
   :emptyp)
  (:nicknames :ir1)
  (:import-from :hindley-milner/subst) ;; for `RECURSE-ON-SLOTS'
  (:import-from :hindley-milner/syntax
                :literal :clause)
  (:shadow :funcall :lambda :quote :if :prog2 :variable)
  (:reexport :hindley-milner/ir1/type)
  (:import-from :hindley-milner/primop
   :operator)
  (:export
   :expr :expr-type
   :variable :variable-name
   :quote :quote-it
   :funcall :funcall-function :funcall-args
   :lambda  :lambda-bindings :lambda-body
   :poly-let :poly-let-binding :poly-let-scheme :poly-let-initform :poly-let-body
   :if :if-predicate :if-then-case :if-else-case
   :primop :primop-op :primop-args
   :prog2 :prog2-side-effect :prog2-return-value

   :parse :parse-program))
(cl:in-package :hindley-milner/ir1)

;;;; transformations from surface-syntax to ir1:
;; - implicit progns (e.g. in let or lambda) are made explicit
;; 
;; - literals are tagged with a quote

(defenum expr ((type type :may-init-unbound t))
    ((variable ((name symbol)))
     (quote ((it syntax:literal)))
     (funcall ((function expr)
               (args (vector expr))))
     (lambda ((bindings (vector symbol))
              (body expr)))
     (poly-let ((binding symbol)
                (scheme type-scheme :may-init-unbound t)
                (initform expr)
                (body expr)))
     (if ((predicate expr)
          (then-case expr)
          (else-case expr)))
     (primop ((op operator)
              (args (vector expr))))
     (prog2 ((side-effect expr)
             (return-value expr)))))

(subst:recurse-on-slots expr
  type)
;; this method is superseded by those below, so each
;; `RECURSE-ON-SLOTS' below must also list `TYPE'
(subst:recurse-on-slots funcall
  type function args)
(subst:recurse-on-slots lambda
  type bindings body)
(subst:recurse-on-slots poly-let
  type scheme initform body)
(subst:recurse-on-slots if
  type predicate then-case else-case)
(subst:recurse-on-slots primop
  type args)
(subst:recurse-on-slots prog2
  type side-effect return-value)

(defgeneric parse (clause)
  (:documentation "transform a `SYNTAX:CLAUSE' into an `IR1:CLAUSE'"))

(declaim (ftype (function ((vector clause)) (values expr &optional))
                transform-implicit-progn))
(defun transform-implicit-progn (progn)
  (cond ((emptyp progn) (error "empty implicit progn"))
        ;; if there's only one clause, you can skip consing the progn
        ((= (length progn) 1) (parse (aref progn 0)))
        (:otherwise
         (flet ((reduce-prog2 (side-effect return-value)
                  (make-instance 'prog2
                                 :side-effect side-effect
                                 :return-value return-value)))
           (reduce #'reduce-prog2 progn :key #'parse)))))

(defmethod parse ((arg-vec vector))
  (iter
    (for arg in-vector arg-vec)
    (collect (parse arg) result-type (vector expr))))

(defmethod parse ((funcall syntax:funcall))
  (make-instance 'funcall
                 :function (parse (syntax:funcall-function funcall))
                 :args (parse (syntax:funcall-args funcall))))

(defmethod parse ((lambda syntax:lambda))
  (make-instance 'lambda
                 :bindings (make-array (length (syntax:lambda-bindings lambda))
                                       :element-type 'symbol
                                       :initial-contents (syntax:lambda-bindings lambda))
                 :body (transform-implicit-progn (syntax:lambda-body lambda))))

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
  "transform (let ((a b) (c d)) e f) into (let a b (let c d (progn e f)))"
  (reduce #'let-from-definition (syntax:let-bindings let)
          :from-end t
          :initial-value (transform-implicit-progn (syntax:let-body let))))

(defmethod parse ((if syntax:if))
  "boring. recurse on `PARSE'"
  (make-instance 'if
                 :predicate (parse (syntax:if-predicate if))
                 :then-case (parse (syntax:if-then-case if))
                 :else-case (parse (syntax:if-else-case if))))

(defmethod parse ((primop syntax:primop))
  "boring. recurse on `PARSE'"
  (make-instance 'primop
                 :op (syntax:primop-op primop)
                 :args (parse (syntax:primop-args primop))))

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
