(uiop:define-package :hindley-milner/closure
  (:mix
   :hindley-milner/cps
   :hindley-milner/prologue
   :iterate
   :cl)
  (:import-from :hindley-milner/primop
   :make-closure-env :access-closure-env)
  (:import-from :gefjon-utils
   :specialized-vector :adjustable-vector :make-adjustable-vector :|:| :-> :shallow-copy)
  (:shadow :sequence)
  (:import-from :alexandria
   :make-gensym)
  (:import-from :trivial-types
   :proper-list)
  (:shadowing-import-from :generic-cl
   :make-hash-map :ensure-get)
  (:export :make-closures-explicit))
(cl:in-package :hindley-milner/closure)

(deftype closure-env ()
  '(adjustable-vector variable))

(deftype local-vars ()
  '(proper-list variable))

(deftype function-closure-vars ()
  '(hash-map-of variable variable))

(deftype sequence (&optional element-type)
  (declare (ignore element-type))
  'cl:sequence)

(defvar *closure-arg*)
(defvar *closure-env*)
(defvar *function-closure-vars*)

(|:| #'closure-var-for-func (-> (variable) variable))
(defun closure-var-for-func (func-name)
  (values (ensure-get func-name *function-closure-vars*
                     (make-instance 'variable
                                    :name (make-gensym 'closure-env)
                                    :type :closure-env))))

(defgeneric collect-closure-vars (expr &optional local-vars)
  (:documentation "returns a new `EXPR' that is like `EXPR'

except that any variable not found in `LOCAL-VARS' has been
added to `CLOSURE-ENV'.

`LOCAL-VARS' should be a cl-style set (i.e. a list of unique elements), and `CLOSURE-ENV' should be an `ADJUSTABLE-VECTOR'"))

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

(|:| #'look-up-in-closure (-> (expr variable) const))
(defun look-up-in-closure (body-expr variable)
  (let* ((idx-var (make-instance 'variable
                                 :name (make-gensym 'index)
                                 :type :fixnum))
         (access-args (specialized-vector variable
                                          *closure-arg*
                                          idx-var))
         (let-form (make-instance 'let
                                  :var variable
                                  :prim-op 'access-closure-env
                                  :args access-args
                                  :in body-expr))
         (index (closure-env-index variable))
         (const-form (make-instance 'const
                                    :name idx-var
                                    :value index
                                    :in let-form)))
    const-form))

(|:| #'look-up-all (-> (expr (proper-list variable)) expr))
(defun look-up-all (body-expr closure-vars)
  (iter
    (with body = body-expr)
    (for var in closure-vars)
    (setf body (look-up-in-closure body var))
    (finally (return body))))

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
     (access-closure-vars new-let
                          (let-args expr)
                          local-vars)))

(defmethod collect-closure-vars ((expr const) &optional local-vars)
  (let* ((body-vars (cons (const-name expr) local-vars))
         (new-body (collect-closure-vars (const-in expr)
                                         body-vars)))
    (shallow-copy expr
                  :in new-body)))

(|:| #'convert-func-body (-> ((or func cont)
                              closure-env
                              local-vars
                              expr
                              expr
                              variable)
                             let))
(defun convert-func-body (old-expr inner-env inner-locals old-body new-in func-name)
  (let* ((*closure-env* inner-env)
         (*closure-arg* (closure-var-for-func func-name))
         (new-body (collect-closure-vars old-body inner-locals))
         (new-func (shallow-copy old-expr
                                 :closure-arg *closure-arg*
                                 :body new-body
                                 :in new-in)))
    (make-instance 'let
                   :var *closure-arg*
                   :prim-op 'make-closure-env
                   :args inner-env
                   :in new-func)))

(defmethod collect-closure-vars ((expr cont) &optional local-vars)
  (let* ((new-locals (cons (cont-name expr) local-vars))
         (new-in (collect-closure-vars (cont-in expr)
                                       new-locals))
         (inner-env (make-adjustable-vector :element-type variable))
         (inner-locals (list (cont-arg expr)))
         (new-cont (convert-func-body expr
                                      inner-env
                                      inner-locals
                                      (cont-body expr)
                                      new-in
                                      (cont-name expr))))
    (access-closure-vars new-cont inner-env local-vars)))

(defmethod collect-closure-vars ((expr func) &optional local-vars)
  (let* ((new-locals (cons (func-name expr) local-vars))
         (new-in (collect-closure-vars (func-in expr)
                                       new-locals))
         (inner-env (make-adjustable-vector :element-type variable))
         (inner-locals (cons (func-continuation-arg expr)
                             (coerce (func-arglist expr) 'list)))
         (new-func (convert-func-body expr
                                      inner-env
                                      inner-locals
                                      (func-body expr)
                                      new-in
                                      (func-name expr))))
    (access-closure-vars new-func inner-env local-vars)))

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
                                (apply-args expr)))
         (closure-env (closure-var-for-func (apply-func expr)))
         (new-apply (shallow-copy expr
                                  :closure-env closure-env)))
    (access-closure-vars new-apply
                         all-args
                         local-vars)))

(defmethod collect-closure-vars ((expr throw) &optional local-vars)
  (let* ((all-args (list (throw-cont expr) (throw-arg expr)))
         (closure-env (closure-var-for-func (throw-cont expr)))
         (new-throw (shallow-copy expr
                                  :closure-env closure-env)))
    (access-closure-vars new-throw
                         all-args
                         local-vars)))

(defun make-closures-explicit (program)
  (let* ((local-vars (list *exit-continuation*))
         (*closure-env* (make-adjustable-vector :element-type variable))
         (*function-closure-vars* (make-hash-map :test #'eq))
         (new-program (collect-closure-vars program local-vars)))
    (assert (uiop:emptyp *closure-env*))
    new-program))
