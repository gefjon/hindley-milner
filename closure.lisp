(uiop:define-package :hindley-milner/closure
  (:mix
   :hindley-milner/cps
   :hindley-milner/prologue
   :iterate
   :cl)
  (:import-from :genhash
   :make-generic-hash-table :hashref :generic-hash-table-count)
  (:shadow :sequence)
  (:import-from :alexandria
   :make-gensym)
  (:import-from :trivial-types
   :proper-list)
  (:export :make-closures-explicit))
(cl:in-package :hindley-milner/closure)

(deftype local-vars ()
  '(proper-list local))

(define-special *closure-env* closure-env
    "a `CLOSURE-ENV' which will map variables for the current function to close over to enclosed variables.")

(defmacro with-closure-env (&body body)
  `(let* ((*closure-env* (make-generic-hash-table :test #'eq)))
     ,@body))

(define-special *local-vars* local-vars
    "a `LOCAL-VARS' (ie cl-style list-as-set) of `LOCAL's which are bound and need not be closed over.")

(defmacro with-locals (locals &body body)
  `(let* ((*local-vars* ,locals))
     ,@body))

(defmacro with-additional-local (var &body body)
  `(with-locals (cons ,var *local-vars*) ,@body))

(defgeneric collect-closure-vars (expr)
  (:documentation "returns a new `EXPR' that is like EXPR,

except that any free variable (i.e. not found in `*LOCAL-VARS*') has
been transformed into a `CLOSURE' and its original has been added to
`*CLOSURE-ENV*'.

`*LOCAL-VARS*' should be a cl-style set (i.e. a list of unique
elements)"))

(|:| #'already-enclosed-p (-> (local) boolean))
(defun already-enclosed-p (var)
  (multiple-value-bind (closure present-p) (hashref var *closure-env*)
    (declare (ignore closure))
    present-p))

(|:| #'should-close-over (-> (variable) boolean))
(defun should-close-over (variable)
  (and (typep variable 'local)
       (not (member variable *local-vars*))))

(|:| #'make-closure-for-local (-> (local) closure))
(defun make-closure-for-local (local)
  (make-instance 'closure
                 :name (name local)
                 :type (type local)))

(|:| #'enclose-var (-> (variable) variable))
(defun enclose-var (var)
  "returns a `VARIABLE' that is suitable to replace VAR"
  (cl:if (should-close-over var)
         (ensure-get var *closure-env* (make-closure-for-local var))
         var))

(|:| #'enclose-arglist (-> ((vector variable)) (vector variable)))
(defun enclose-arglist (arglist)
  (map '(vector variable) #'enclose-var arglist))

(defmethod collect-closure-vars ((expr let))
  (shallow-copy expr
                :in (with-additional-local (var expr)
                      (collect-closure-vars (in expr)))
                :args (enclose-arglist (args expr))))

(|:| #'convert-func-body (-> (procedure local-vars) procedure))
(defun convert-func-body (defn inner-locals)
  (with-closure-env
    (with-locals inner-locals
      (shallow-copy defn
                    :body (collect-closure-vars (body defn))
                    :closes-over *closure-env*))))

(defgeneric closurify-defn (defn)
  (:documentation "returns a new `DEFINITION' that is like DEFN but has been converted to have explicit closure vars."))

(defmethod collect-closure-vars ((expr bind))
  (shallow-copy expr
                :defn (closurify-defn (defn expr))
                :in (with-additional-local (name (defn expr))
                      (collect-closure-vars (in expr)))))

(defmethod closurify-defn ((defn continuation))
  (convert-func-body defn (list (arg defn))))

(defmethod closurify-defn ((defn func))
  (convert-func-body defn (cons (arglist defn)
                                (coerce (arglist defn) 'list))))

(defmethod closurify-defn ((defn constant))
  defn)


(defmethod collect-closure-vars ((expr if))
  (shallow-copy expr
                :predicate (enclose-var (predicate expr))
                :then-clause (collect-closure-vars (then-clause expr))
                :else-clause (collect-closure-vars (else-clause expr))))

(defmethod collect-closure-vars ((expr apply))
  (shallow-copy expr
                :func (enclose-var (func expr))
                :args (enclose-arglist (args expr))
                :continuation (enclose-var (continuation expr))))

(defmethod collect-closure-vars ((expr throw))
  (shallow-copy expr
                :cont (enclose-var (cont expr))
                :arg (enclose-var (arg expr))))

(defun make-closures-explicit (program)
  (with-closure-env
    (with-locals ()
      (let* ((new-program (collect-closure-vars program)))
        (assert (zerop (generic-hash-table-count *closure-env*)))
        new-program))))
