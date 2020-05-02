(uiop:define-package :hindley-milner/closure
  (:mix
   :hindley-milner/cps
   :hindley-milner/prologue
   :iterate
   :cl)
  (:shadow :sequence)
  (:import-from :alexandria
   :make-gensym)
  (:import-from :trivial-types
   :proper-list)
  (:export :make-closures-explicit :access-closure-env :access-closure-env-index))
(cl:in-package :hindley-milner/closure)

(define-class access-closure-env
    ((index fixnum))
  :superclasses (definition))

(deftype closure-env ()
  '(adjustable-vector variable))

(deftype local-vars ()
  '(proper-list variable))

(|:| *closure-env* closure-env)
(defvar *closure-env*)
(setf (documentation '*closure-env* 'cl:variable)
      "a `CLOSURE-ENV' which will be filled with variables for the
      current function to close over.")

(defmacro with-compiler-toplevel-state (&body body)
  `(let* ((*closure-env* (make-adjustable-vector :element-type variable)))
     ,@body))

(defgeneric collect-closure-vars (expr &optional local-vars)
  (:documentation "returns a new `EXPR' that is like EXPR,

except that any free variable (i.e. not found in `LOCAL-VARS') has
been transformed into a closure access and added to `*CLOSURE-ENV*'.

`LOCAL-VARS' should be a cl-style set (i.e. a list of unique elements)"))

(|:| #'already-enclosed-p (-> (variable) boolean))
(defun already-enclosed-p (var)
  (iter
    (for closure-var in-vector *closure-env*)
    (when (eq var closure-var)
      (return t))
    (finally (return nil))))

(|:| #'should-close-over (-> (variable list) boolean))
(defun should-close-over (variable local-vars)
  (not (or (member variable local-vars)
           (already-enclosed-p variable))))

(|:| #'closure-env-index (-> (variable) unsigned-byte))
(defun closure-env-index (variable)
  (or (position variable *closure-env* :test #'eq)
      (error "variable ~s not present in closure-env ~s" variable *closure-env*)))

(|:| #'add-var (-> (variable) (values &optional)))
(defun add-var (variable)
  (vector-push-extend variable *closure-env*)
  (values))

(|:| #'add-all (-> ((proper-list variable)) (values &optional)))
(defun add-all (vars)
  (mapc #'add-var vars)
  (values))

(|:| #'look-up-in-closure (-> (expr variable) bind))
(defun look-up-in-closure (body-expr variable)
  (let* ((index (closure-env-index variable))
         (access (make-instance 'access-closure-env
                                :index index
                                :name variable)))
    (make-instance 'bind
                   :defn access
                   :in body-expr)))

(|:| #'access-closure-vars (-> (expr
                                (sequence variable)
                                local-vars)
                               expr))
(defun access-closure-vars (body-expr vars-to-access local-vars)
  (iter
    (with body = body-expr)
    (with already-accessed = ())
    (for var in-sequence vars-to-access)
    (when (member var already-accessed)
      (next-iteration))
    (setf already-accessed (cons var already-accessed))
    (when (should-close-over var local-vars)
      (add-var var))
    (unless (member var local-vars)
      (setf body (look-up-in-closure body var)))
    (finally (return body))))

(defmethod collect-closure-vars ((expr let) &optional local-vars)
  (let* (;; note: prior to the closure transform, all bindings bind
         ;; unique vars, so this will still be a set
         (body-vars (cons (let-var expr) local-vars))
         (new-body (collect-closure-vars (let-in expr)
                                         body-vars))
         (new-let (shallow-copy expr
                                :in new-body)))
    ;; also note: prior to the closure transform, no let-arg will ever
    ;; be a fixnum; they will all be variables.
    (access-closure-vars new-let
                         (let-args expr)
                         local-vars)))

(|:| #'convert-func-body (-> (function-like-definition
                              local-vars)
                             function-like-definition))
(defun convert-func-body (old-defn inner-locals)
  (let* ((*closure-env* (adjustable-vector variable))
         (old-body (function-like-definition-body old-defn))
         (new-body (collect-closure-vars old-body inner-locals)))
     (shallow-copy old-defn
                   :body new-body
                   :closes-over *closure-env*)))

(defgeneric closurify-defn (defn)
  (:documentation "returns a new `DEFINITION' that is like DEFN but has been converted to have explicit closure vars."))

(defmethod collect-closure-vars ((expr bind) &optional local-vars)
  (let* ((defn (bind-defn expr))
         (new-locals (cons (definition-name defn) local-vars))
         (new-in (collect-closure-vars (bind-in expr)
                                       new-locals))
         (new-defn (closurify-defn defn))
         (new-bind (make-instance 'bind
                                  :defn new-defn
                                  :in new-in))
         (new-vars-needed (when (typep new-defn 'function-like-definition)
                            (function-like-definition-closes-over new-defn))))
    (access-closure-vars new-bind new-vars-needed local-vars)))

(defmethod closurify-defn ((defn contdefn))
  (convert-func-body defn (list (contdefn-arg defn))))

(defmethod closurify-defn ((defn fdefn))
  (convert-func-body defn (cons (fdefn-continuation-arg defn)
                                (coerce (fdefn-arglist defn) 'list))))

(defmethod closurify-defn ((defn constdefn))
  defn)


(defmethod collect-closure-vars ((expr if) &optional local-vars)
  (let* ((new-then (collect-closure-vars (if-then-clause expr)
                                         local-vars))
         (new-else (collect-closure-vars (if-else-clause expr)
                                         local-vars))
         (new-if (shallow-copy expr
                               :then-clause new-then
                               :else-clause new-else)))
    (access-closure-vars new-if
                         (list (if-predicate expr))
                         local-vars)))

(defmethod collect-closure-vars ((expr apply) &optional local-vars)
  (let* ((all-args (concatenate 'list
                                (list (apply-func expr)
                                      (apply-continuation expr))
                                (apply-args expr))))
    (access-closure-vars expr
                         all-args
                         local-vars)))

(defmethod collect-closure-vars ((expr throw) &optional local-vars)
  (let* ((all-args (list (throw-cont expr) (throw-arg expr))))
    (access-closure-vars expr
                         all-args
                         local-vars)))

(defun make-closures-explicit (program)
  (with-compiler-toplevel-state
    (let* ((local-vars (list *exit-continuation*))
           (new-program (collect-closure-vars program local-vars)))
      (assert (uiop:emptyp *closure-env*))
      new-program)))
