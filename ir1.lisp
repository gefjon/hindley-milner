(uiop:define-package :hindley-milner/ir1
    (:use :hindley-milner/defenum :trivial-types :cl)
  (:shadowing-import-from :hindley-milner/typecheck/type
                          :type)
  (:nicknames :ir1)
  (:import-from :hindley-milner/syntax
                :literal :clause)
  (:shadow :funcall :lambda :let :quote :if :binop :prog2 :variable)
  (:export

   :typed-node :typed-node-type :type-already-computed-p

   :expr
   :variable :make-variable :variable-name
   :quote :make-quote :quote-it
   :funcall :make-funcall :funcall-function :funcall-arg
   :lambda :make-lambda :lambda-binding :lambda-body
   :let :make-let :let-binding :let-initform :let-body
   :if :make-if :if-predicate :if-then-case :if-else-case
   :binop :make-binop :binop-op :binop-lhs :binop-rhs
   :prog2 :make-prog2 :prog2-side-effect :prog2-return-value

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

(defclass typed-node ()
  ((type :accessor typed-node-type
         :type type)))

(declaim (ftype (function (typed-node) boolean)
                type-already-computed-p))
(defun type-already-computed-p (typed-node)
  (slot-boundp typed-node 'type))

(defmacro derive-print-object-for-expr (name slots)
  (alexandria:with-gensyms (this stream)
    (cl:let ((slot-names (mapcar #'first slots)))
      (labels ((print-slot-and-name (name value-form)
                 `(cl:prog2
                      (pprint-newline :linear ,stream)
                      (format ,stream "~a: ~a;" ',name ,value-form)))
               (print-slot-form (slot)
                 (print-slot-and-name slot slot)))
        `(defmethod print-object ((,this ,name) ,stream)
           (pprint-logical-block (,stream nil)
             (print-unreadable-object (,this ,stream :type t :identity nil)
               (with-slots ,slot-names ,this
                 (when (type-already-computed-p ,this)
                   ,(print-slot-and-name 'type `(typed-node-type ,this)))
                 ,@(mapcar #'print-slot-form slot-names)))))))))

(defmacro defexpr (name slots)
  `(prog1
       (gefjon-utils:defclass ,name ,slots :superclasses (typed-node))
     (derive-print-object-for-expr ,name ,slots)))

(defenum expr
    ((variable ((name symbol)))
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
     (prog2 ((side-effect expr)
             (return-value expr))))
  :defstruct defexpr)

(defgeneric parse (clause)
  (:documentation "transform a `SYNTAX:CLAUSE' into an `IR1:CLAUSE'"))

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
         (reduce #'make-prog2 progn :key #'parse))))

(defmethod parse ((funcall syntax:funcall))
  "transform (funcall a b c) into (funcall (funcall a b) c)

TODO: handle the edge case of invoking a function on no arguments by transforming into an invocation of one empty argument"
  (reduce #'make-funcall (mapcar #'parse (syntax:funcall-args funcall))
          :initial-value (parse (syntax:funcall-function funcall))))

(defmethod parse ((lambda syntax:lambda))
  "transform (lambda (a b) (c d)) into (lambda a (lambda b (progn c d)))

TODO: correctly handle the edge case where (lambda-bindings lambda) is nil by transforming into a function of one empty argument"
  (flet ((reduce-lambda (binding body)
           (make-lambda binding body)))
    (reduce #'reduce-lambda (syntax:lambda-bindings lambda)
            :from-end t
            :initial-value (transform-implicit-progn (syntax:lambda-body lambda)))))

(declaim (ftype (function (syntax:definition expr) expr)
                let-from-definition))
(defun let-from-definition (definition body)
  "build an `IR1:LET' from a `SYNTAX:DEFINITION' and an `IR1:EXPR'

passed to `REDUCE' in parsers for `LET' and `SYNTAX:PROGRAM'"
  (make-let (syntax:definition-binding definition)
            (parse (syntax:definition-value definition))
            body))

(defmethod parse ((let syntax:let))
  "transform (let ((a b) (c d)) e f) into (let a b (let c d (progn e f)))

edge case: parses (let () a b) into (progn a b)"
  (reduce #'let-from-definition (syntax:let-bindings let)
          :from-end t
          :initial-value (transform-implicit-progn (syntax:let-body let))))

(defmethod parse ((if syntax:if))
  "boring. recurse on `PARSE'"
  (make-if (parse (syntax:if-predicate if))
           (parse (syntax:if-then-case if))
           (parse (syntax:if-else-case if))))

(defmethod parse ((binop syntax:binop))
  "boring. recurse on `PARSE'"
  (make-binop (parse (syntax:binop-op binop))
              (parse (syntax:binop-lhs binop))
              (parse (syntax:binop-rhs binop))))

(defmethod parse ((symbol symbol))
  "return variables unchanged; quote booleans"
  (etypecase symbol
    ;; quote `CL:BOOLEAN's, i.e. the symbols `CL:T' and `CL:NIL'
    (boolean (make-quote symbol))
    ;; transform and quote `SYNTAX:BOOLEAN-LITERAL's, i.e. the symbols
    ;; `HM:|true|' and `HM:|false|', even though they should already
    ;; be transformed by `SYNTAX:PARSE'
    (syntax:boolean-literal (make-quote (syntax:parse-boolean symbol)))
    (symbol (make-variable symbol))))

(defmethod parse (clause)
  "quote literals; error otherwise"
  (etypecase clause
    (syntax:literal (make-quote clause))))

(declaim (ftype (function (syntax:program) expr)
                parse-program))
(defun parse-program (program)
  (reduce #'let-from-definition (syntax:program-definitions program)
          :from-end t
          :initial-value (parse (syntax:program-entry program))))
