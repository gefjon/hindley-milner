(uiop:define-package :hindley-milner/syntax/parse
  (:mix :hindley-milner/syntax/clause :iterate :cl)
  (:import-from :alexandria :with-gensyms)
  (:import-from :gefjon-utils :specialized-vector :compiler-state)
  (:export
   :parse :parse-def))
(cl:in-package :hindley-milner/syntax/parse)

(defgeneric parse (thing)
  (:documentation "parse an object produced by `READ' into a `SYNTAX:CLAUSE'"))

(defgeneric parse-list (head &rest stuff)
  (:documentation "intended to be `CL:APPLY'ed to a `TRIVIAL-TYPES:PROPER-LIST'"))

(defmethod parse ((thing cons))
  (apply #'parse-list thing))

(defun parse-body (body)
  (iter (for clause in body)
    (collect (parse clause) result-type (vector clause))))

(defun parse-def (definition)
  (destructuring-bind (def binding value) definition
    (unless (eq def 'hm:|def|)
      (error "~a is not a definition" definition))
    (check-type binding symbol)
    (make-instance 'definition
                   :binding binding
                   :value (parse value))))

(compiler-state
  (defun get-hm-symbol (symbol)
    "return a symbol that is like SYMBOL, but downcased and interend in `:HM'"
    (intern (string-downcase (symbol-name symbol))
            (find-package :hm))))

(defmacro defparse (head lambda-list &body body)
  "define a parser for lists starting with HEAD and continuing with LAMBDA-LIST

e.g. (defparse funcall (function &rest args)
       (make-instance 'funcall
                      :function (parse function)
                      :args (mapcar #'parse args)))"
  (cl:let ((head (get-hm-symbol head)))
    (with-gensyms (head-arg stuff-arg)
      `(defmethod parse-list ((,head-arg (eql ',head)) &rest ,stuff-arg)
         (declare (ignorable ,head-arg))
         ,(format nil "parser for ~s generated by `HINDLEY-MILNER/SYNTAX:DEFPARSE'" head)
         (destructuring-bind ,lambda-list ,stuff-arg
           ,@body)))))

(defmethod parse ((thing (eql 'hm:|true|)))
  (declare (ignorable thing))
  (make-instance 'quote :it t))
(defmethod parse ((thing (eql 'hm:|false|)))
  (declare (ignorable thing))
  (make-instance 'quote :it nil))
(defmethod parse ((thing fixnum))
  (make-instance 'quote :it thing))
(defmethod parse ((symbol symbol))
  (make-instance 'variable :name symbol))

(defmethod parse-list ((head symbol) &rest args)
  "fallthrough method for named funcalls, so that (foo bar baz) parses as (funcall foo bar baz)"
  (cl:if (typep head 'primop:operator)
         (make-instance 'primop
                        :op head
                        :args (parse-body args))
         (make-instance 'funcall
                        :function (parse head)
                        :args (parse-body args))))

(defparse funcall (function &rest args)
  (make-instance 'funcall
                 :function (parse function)
                 :arg (parse-body args)))

(defparse lambda (lambda-list &body body)
  (flet ((check-symbol (symbol)
           (unless (typep symbol 'symbol)
             (error "non-symbol ~s in `LAMBDA-LIST'" symbol))
           symbol))
    (make-instance 'lambda
                   :bindings (map '(vector symbol) #'check-symbol lambda-list)
                   :body (parse-body body))))

(defparse let (bindings &body body)
  (flet ((parse-binding (binding)
           (destructuring-bind (symbol value) binding
             (check-type symbol symbol)
             (make-instance 'definition
                            :binding symbol
                            :value (parse value)))))
    (make-instance 'let
                   :bindings (map '(vector definition) #'parse-binding bindings)
                   :body (parse-body body))))

(defparse if (predicate then-clause else-clause)
  (make-instance 'if
                 :predicate (parse predicate)
                 :then-case (parse then-clause)
                 :else-case (parse else-clause)))