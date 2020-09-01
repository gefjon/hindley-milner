(uiop:define-package :hindley-milner/cps/closure
  (:mix
   :hindley-milner/cps/type
   :hindley-milner/cps/expr
   :hindley-milner/prologue
   :iterate
   :cl)
  (:import-from :trivial-types
   :proper-list)
  (:export :make-closures-explicit))
(cl:in-package :hindley-milner/cps/closure)

(deftype local-vars ()
  '(proper-list local))

(define-special *closure-env* closure-vars
    "a `CLOSURE-ENV' which will map variables for the current function to close over to enclosed variables.")

(defmacro with-closure-env (&body body)
  `(let* ((*closure-env* (adjustable-vector closure)))
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
  (not (not (find var *closure-env* :key #'corresponding-local))))

(|:| #'should-close-over (-> (variable) boolean))
(defun should-close-over (variable)
  (and (typep variable 'local)
       (not (member variable *local-vars*))))

(|:| #'make-closure-for-local (-> (local) closure))
(defun make-closure-for-local (local)
  (make-instance 'closure
                 :name (name local)
                 :type (type local)
                 :corresponding-local local))

(|:| #'enclose-var (-> (variable) variable))
(defun enclose-var (var)
  "returns a `VARIABLE' that is suitable to replace VAR"
  (cl:if (should-close-over var)
         (ensure-find var
                      *closure-env*
                      (make-closure-for-local var)
                      :key #'corresponding-local)
         var))

(|:| #'enclose-arglist (-> ((vector variable)) (vector variable)))
(defun enclose-arglist (arglist)
  (map '(vector variable) #'enclose-var arglist))

(defmethod collect-closure-vars ((expr let))
  (shallow-copy expr
                :in (with-additional-local (var expr)
                      (collect-closure-vars (in expr)))
                :args (enclose-arglist (args expr))))

(defmethod collect-closure-vars ((expr alloc-struct))
  (shallow-copy expr
                :in (with-additional-local (var expr)
                      (collect-closure-vars (in expr)))
                :elts (enclose-arglist (elts expr))))

(defmethod collect-closure-vars ((expr read-struct))
  (shallow-copy expr
                :in (with-additional-local (var expr)
                      (collect-closure-vars (in expr)))
                :src (enclose-var (src expr))))

(defmethod collect-closure-vars ((expr transmute))
  (shallow-copy expr
                :in (with-additional-local (new expr)
                      (collect-closure-vars (in expr)))
                :old (enclose-var (old expr))))

(|:| #'enclose-cenv (-> ((vector closure)) (vector closure)))
(defun enclose-cenv (cenv)
  (flet ((enclose-closure-source (closure)
           (let* ((source (corresponding-local closure))
                  (enclosed (enclose-var source)))
             (cl:if (eq enclosed source) closure
                    (shallow-copy closure
                                  :corresponding-local enclosed)))))
    (map '(vector variable) #'enclose-closure-source cenv)))

(defmethod collect-closure-vars ((expr proc))
  (multiple-value-bind (new-body cenv)
      (with-closure-env
        (with-locals (coerce (arglist expr) 'list)
          (values
           (collect-closure-vars (body expr))
           *closure-env*)))
    (shallow-copy expr
                  :body new-body
                  :closes-over (enclose-cenv cenv)
                  :in (with-additional-local (name expr)
                        (collect-closure-vars (in expr))))))

(defmethod collect-closure-vars ((expr if))
  (shallow-copy expr
                :predicate (enclose-var (predicate expr))
                :then-clause (collect-closure-vars (then-clause expr))
                :else-clause (collect-closure-vars (else-clause expr))))

(defmethod collect-closure-vars ((expr apply))
  (shallow-copy expr
                :func (enclose-var (func expr))
                :args (enclose-arglist (args expr))))

(defun make-closures-explicit (program)
  (with-closure-env
    (with-locals (list *exit-continuation*)
      (let* ((new-program (collect-closure-vars program)))
        (assert (zerop (length *closure-env*)) ()
                "expect no top-level closure vars but found ~a"
                *closure-env*)
        new-program))))
