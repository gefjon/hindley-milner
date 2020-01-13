(uiop:define-package :hindley-milner/syntax
  (:nicknames :syntax)
  (:use :iterate :hindley-milner/defenum :trivial-types :cl :named-readtables)
  (:shadow :variable :quote :funcall :lambda :let :quote :if :binop)
  (:import-from :alexandria)
  (:export

   :literal
   :boolean-literal
   :parse-boolean
   :operator

   :definition :definition-binding :definition-value
   :parse-def

   :clause
   :variable :variable-name
   :quote :quote-it
   :funcall :funcall-function :funcall-args
   :lambda :lambda-bindings :lambda-body
   :let :let-bindings :let-body
   :if :if-predicate :if-then-case :if-else-case
   :binop :binop-op :binop-lhs :binop-rhs

   :parse

   :hindley-milner ;; names the readtable

   :program :program-definitions :program-entry
   :read-program-from-file))
(cl:in-package :hindley-milner/syntax)

(defpackage :hindley-milner
  (:export :|def|
           :|funcall|
           :|lambda|
           :|let|
           :|if|
           :|binop|

           :+ :- :* :/ :=

   :|boolean| :|true| :|false|

   :|fixnum|)
  (:nicknames :hm))

(deftype boolean-literal ()
  "converted at parse-time into a `CL:BOOLEAN' i.e. either `T' or `NIL'"
  '(member hm:|true| hm:|false|))

(defun parse-boolean (bool)
    (ecase bool
      (hm:|true| t)
      (hm:|false| nil)))

;; booleans aren't really literals, because there's no way to get the
;; reader to produce one in a hindley-milner source file. this is in
;; contrast with other kinds of literals, which are produced by the
;; reader and unchanged by parsing.
(deftype literal () '(or fixnum boolean))

(deftype operator ()
  '(member hm:+ hm:- hm:* hm:/ hm:=))

(gefjon-utils:defclass definition
  ((binding symbol)
   (value clause)))

(defun parse-def (definition)
  (destructuring-bind (def binding value) definition
    (unless (eq def 'hm:|def|)
      (error "~a is not a definition" definition))
    (check-type binding symbol)
    (make-instance 'definition
                   :binding binding
                   :value (parse value))))

(defenum clause ()
  ((variable ((name symbol)))
   (quote ((it literal)))
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
           (rhs clause)))))

(defgeneric parse (thing)
  (:documentation "parse an object produced by `READ' into a `SYNTAX:CLAUSE'"))

(defmethod parse ((thing (eql 'hm:|true|)))
  (declare (ignorable thing))
  (make-instance 'quote :it t))
(defmethod parse ((thing (eql 'hm:|false|)))
  (declare (ignorable thing))
  (make-instance 'quote :it nil))
(defmethod parse ((thing fixnum))
  (make-instance 'quote :it thing))
(defmethod parse ((thing cons))
  (apply #'parse-list thing))

(defmethod parse ((symbol symbol))
  (make-instance 'variable :name symbol))

(defun parse-body (body)
  (mapcar #'parse body))

(defgeneric parse-list (head &rest stuff)
  (:documentation "intended to be `CL:APPLY'ed to a `TRIVIAL-TYPES:PROPER-LIST'"))

(defmacro defparse (head lambda-list &body body)
  "define a parser for lists starting with HEAD and continuing with LAMBDA-LIST

e.g. (defparse funcall (function &rest args)
       (make-instance 'funcall
                      :function (parse function)
                      :args (mapcar #'parse args)))"
  (cl:let ((head (intern (string-downcase (symbol-name head))
                         (find-package :hindley-milner))))
    (alexandria:with-gensyms (head-arg stuff-arg)
      `(defmethod parse-list ((,head-arg (eql ',head)) &rest ,stuff-arg)
         (declare (ignorable ,head-arg))
         ,(format nil "parser for ~s generated by `HINDLEY-MILNER/SYNTAX:DEFPARSE'" head)
         (destructuring-bind ,lambda-list ,stuff-arg
           ,@body)))))

(defmethod parse-list ((head symbol) &rest args)
  "fallthrough method for named funcalls, so that (foo bar baz) parses as (funcall foo bar baz)"
  (make-instance 'funcall
                 :function (parse head)
                 :args (parse-body args)))

(defparse funcall (function &rest args)
  (make-instance 'funcall
                 :function (parse function)
                 :arg (parse-body args)))

(defparse lambda (lambda-list &body body)
  (flet ((check-symbol (symbol)
           (unless (typep symbol 'symbol)
             (error "non-symbol ~s in `LAMBDA-LIST'" symbol))))
    (make-instance 'lambda
                   :bindings (mapc #'check-symbol lambda-list)
                   :body (parse-body body))))

(defparse let (bindings &body body)
  (flet ((parse-binding (binding)
           (destructuring-bind (symbol value) binding
             (check-type symbol symbol)
             (make-instance 'definition
                            :binding symbol
                            :value (parse value)))))
    (make-instance 'let
                   :bindings (mapcar #'parse-binding bindings)
                   :body (parse-body body))))

(defparse if (predicate then-clause else-clause)
  (make-instance 'if
                 :predicate (parse predicate)
                 :then-case (parse then-clause)
                 :else-case (parse else-clause)))

(defparse binop (op lhs rhs)
  (check-type op operator)
  (make-instance 'binop
                 :op op
                 :lhs (parse lhs)
                 :rhs (parse rhs)))

(gefjon-utils:defclass program
  ((definitions (proper-list definition))
   (entry clause)))

(defreadtable hindley-milner
  (:merge :standard)
  (:case :preserve))

(defun read-program-from-file (file)
  (cl:let ((*readtable* (find-readtable 'hindley-milner))
           (*package* (find-package :hindley-milner)))
    (iter (with definitions)
      (with entry)
      (for form in-file file)
      (when entry
        (push (parse-def entry) definitions))
      (setf entry form)
      (finally (return
                 (make-instance 'program
                                :definitions (reverse definitions)
                                :entry (parse entry)))))))
