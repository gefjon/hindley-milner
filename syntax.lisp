(uiop:define-package :hindley-milner/syntax
  (:nicknames :syntax)
  (:use :iterate :hindley-milner/defenum :trivial-types :cl :named-readtables)
  (:shadow :funcall :lambda :let :quote :if :binop)
  (:import-from :alexandria)
  (:export

   :literal
   :operator

   :definition :make-definition :definition-binding :definition-value
   :parse-def

   :clause
   :funcall :make-funcall :funcall-function :funcall-args
   :lambda :make-lambda :lambda-bindings :lambda-body
   :let :make-let :let-bindings :let-body
   :if :make-if :if-predicate :if-then-case :if-else-case
   :binop :make-binop :binop-op :binop-lhs :binop-rhs

   :parse

   :hindley-milner ;; names the readtable

   :program :make-program :program-definitions :program-entry
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
;; reader to produce one in a hindley-milner source file (other than
;; typing literally `CL:T' or `CL:NIL'). this is in contrast with
;; other kinds of literals, which are produced by the reader and
;; unchanged by parsing.
(defenum literal fixnum boolean)
(deftype operator ()
  '(member hm:+ hm:- hm:* hm:/ hm:=))

(gefjon-utils:defstruct definition
  ((binding symbol)
   (value clause)))

(defun parse-def (definition)
  (destructuring-bind (def binding value) definition
    (unless (eq def 'hm:|def|)
      (error "~a is not a definition" definition))
    (check-type binding symbol)
    (make-definition binding (parse value))))

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

(defun parse (thing)
  (etypecase thing
    (boolean-literal (parse-boolean thing)) ; keep this clause before `SYMBOL', so `HM:|true|' parses as `CL:T'
    ((or symbol literal) thing)
    (cons (apply #'parse-list thing))))

(defun parse-body (body)
  (mapcar #'parse body))

(defgeneric parse-list (head &rest stuff)
  (:documentation "intended to be `CL:APPLY'ed to a `TRIVIAL-TYPES:PROPER-LIST'"))

(defmacro defparse (head lambda-list &body body)
  "define a parser for lists starting with HEAD and continuing with LAMBDA-LIST

e.g. (defparse funcall (function &rest args)
  (make-funcall (parse function)
                (mapcar #'parse args)))"
  (cl:let ((head (intern (string-downcase (symbol-name head))
                         (find-package :hindley-milner))))
    (alexandria:with-gensyms (head-arg stuff-arg)
      `(defmethod parse-list ((,head-arg (eql ',head)) &rest ,stuff-arg)
         (declare (ignorable ,head-arg))
         ,(format nil "parser for ~s generated by `HINDLEY-MILNER/SYNTAX:DEFPARSE'" head)
         (destructuring-bind ,lambda-list ,stuff-arg
           ,@body)))))

(defparse funcall (function &rest args)
  (make-funcall (parse function)
                (parse-body args)))

(defparse lambda (lambda-list &body body)
  (flet ((check-symbol (symbol)
           (unless (typep symbol 'symbol)
             (error "non-symbol ~s in `LAMBDA-LIST'" symbol))))
    (make-lambda (mapc #'check-symbol lambda-list)
                 (parse-body body))))

(defparse let (bindings &body body)
  (flet ((parse-binding (binding)
           (destructuring-bind (symbol value) binding
             (check-type symbol symbol)
             (make-definition symbol (parse value)))))
    (make-let (mapcar #'parse-binding bindings)
              (parse-body body))))

(defparse if (predicate then-clause else-clause)
  (make-if (parse predicate)
           (parse then-clause)
           (parse else-clause)))

(defparse binop (op lhs rhs)
  (check-type op operator)
  (make-binop op (parse lhs) (parse rhs)))

(gefjon-utils:defstruct program
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
                 (make-program (reverse definitions)
                               (parse entry)))))))
