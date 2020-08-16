(uiop:define-package :hindley-milner/ir1/parse
  (:mix
   :hindley-milner/ir1/type
   :hindley-milner/ir1/expr
   :hindley-milner/prologue
   :cl
   :iterate)
  (:import-from :hindley-milner/syntax)
  (:import-from :uiop :emptyp)
  (:export :parse-program))
(cl:in-package :hindley-milner/ir1/parse)

(defgeneric parse (clause)
  (:documentation "transform a `SYNTAX:CLAUSE' into an `IR1:EXPR'"))

(|:| #'parse-definition (-> (syntax:definition) definition))
(defun parse-definition (definition)
  (make-instance 'definition
                 :name (syntax:binding definition)
                 :initform (parse (syntax:value definition))))

(|:| #'transform-implicit-progn (-> ((vector syntax:clause)) expr))
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
                 :func (parse (syntax:func funcall))
                 :args (parse (syntax:args funcall))))

(defmethod parse ((lambda syntax:lambda))
  (make-instance 'lambda
                 :bindings (make-array (length (syntax:bindings lambda))
                                       :element-type 'symbol
                                       :initial-contents (syntax:bindings lambda))
                 :body (transform-implicit-progn (syntax:body lambda))))

(defmethod parse ((let syntax:let))
  "transform (let ((a b) (c d)) e f) into (let a b (let c d (progn e f)))"
  (flet ((let-from-definition (definition body)
           (make-instance 'let
                          :def (parse-definition definition)
                          :body body)))
    (reduce #'let-from-definition (syntax:bindings let)
            :from-end t
            :initial-value (transform-implicit-progn (syntax:body let)))))

(defmethod parse ((if syntax:if))
  "boring. recurse on `PARSE'"
  (make-instance 'if
                 :predicate (parse (syntax:predicate if))
                 :then-case (parse (syntax:then-case if))
                 :else-case (parse (syntax:else-case if))))

(defmethod parse ((primop syntax:primop))
  "boring. recurse on `PARSE'"
  (make-instance 'primop
                 :op (syntax:op primop)
                 :args (parse (syntax:args primop))))

(defmethod parse ((variable syntax:variable))
  (make-instance 'variable
                 :name (syntax:name variable)))

(defmethod parse ((quote syntax:quote))
  (make-instance 'quote
                 :it (syntax:it quote)))

(|:| #'parse-program (-> (syntax:program) program))
(defun parse-program (program)
  "transform a `SYNTAX:PROGRAM' into an `IR1:EXPR'"
  (make-instance 'program
                 :definitions (map '(vector definition) #'parse-definition
                                   (syntax:definitions program))
                 :entry (parse (syntax:entry program))))
