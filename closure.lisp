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
  (:import-from :alexandria
   :make-gensym)
  (:export :make-closures-explicit))
(cl:in-package :hindley-milner/closure)

(deftype closure-env ()
  '(adjustable-vector variable))

(defgeneric collect-closure-vars (expr &optional closure-env local-vars)
  (:documentation "returns a new `EXPR' that is like `EXPR'

except that any variable not found in `LOCAL-VARS' has been
added to `CLOSURE-ENV'.

`LOCAL-VARS' should be a cl-style set (i.e. a list of unique elements), and `CLOSURE-ENV' should be an `ADJUSTABLE-VECTOR'"))

(|:| #'already-enclosed-p (-> (closure-env variable) boolean))
(defun already-enclosed-p (closure-env var)
  (iter
    (for closure-var in-vector closure-env)
    (when (eq var closure-var)
      (return t))
    (finally (return nil))))

(|:| #'maybe-add-var (-> (variable closure-env list) (values &optional)))
(defun maybe-add-var (variable closure-env local-vars)
  (unless (or (member variable local-vars)
              (already-enclosed-p closure-env variable))
    (vector-push-extend variable closure-env))
  (values))

(defmethod collect-closure-vars ((expr let) &optional closure-env local-vars)
  (iter
    (for arg in-vector (let-args expr))
    (maybe-add-var arg closure-env local-vars)
    (finally
     (let* (;; note: prior to the closure transform, all bindings bind
            ;; unique vars, so this will still be a set
            (body-vars (cons (let-var expr) local-vars))
            (new-body (collect-closure-vars (let-in expr)
                                            closure-env
                                            body-vars)))
       (return (shallow-copy expr
                             :in new-body))))))

(defmethod collect-closure-vars ((expr const) &optional closure-env local-vars)
  (let* ((body-vars (cons (const-name expr) local-vars))
         (new-body (collect-closure-vars (const-in expr)
                                         closure-env
                                         body-vars)))
    (shallow-copy expr
                  :in new-body)))

(defun access-all-closure-vars (body-expr closure-env closure-arg)
  (let* ((index 0))
    (flet ((look-up-in-closure (body-expr variable)
             (let* ((idx-var (make-instance 'variable
                                            :name (make-gensym 'index)
                                            :type :fixnum))
                    (access-args (specialized-vector variable
                                                     closure-arg
                                                     idx-var))
                    (let-form (make-instance 'let
                                             :var variable
                                             :prim-op 'access-closure-env
                                             :args access-args
                                             :in body-expr))
                    (const-form (make-instance 'const
                                               :name idx-var
                                               :value index
                                               :in let-form)))
               (incf index)
               const-form)))
      (reduce #'look-up-in-closure closure-env :initial-value body-expr))))

(|:| #'add-double-closure-vars
     (-> (closure-env (vector variable) list) (values &optional)))
(defun maybe-add-many (closure-env to-add locals)
  (iter
    (for var in-vector to-add)
    (maybe-add-var var closure-env locals))
  (values))

(defmethod collect-closure-vars ((expr cont) &optional closure-env local-vars)
  (let* ((inner-env (make-adjustable-vector :element-type variable))
         (inner-locals (list (cont-arg expr)))
         (new-body (collect-closure-vars (cont-body expr)
                                         inner-env
                                         inner-locals))
         (closure-arg (make-instance 'variable
                                     :name (make-gensym 'closure-env)
                                     :type :closure-env))
         (new-body-with-locals-bound
           (access-all-closure-vars new-body inner-env closure-arg))
         (new-locals (cons (cont-name expr) local-vars))
         (new-in (collect-closure-vars (cont-in expr)
                                       closure-env
                                       new-locals))
         (new-cont (shallow-copy expr
                                 :closure-arg closure-arg
                                 :body new-body-with-locals-bound
                                 :closes-over inner-env
                                 :in new-in)))
    (maybe-add-many closure-env inner-env local-vars)
    new-cont))

(defmethod collect-closure-vars ((expr func) &optional closure-env local-vars)
  (let* ((inner-env (make-adjustable-vector :element-type variable))
         (inner-locals (cons (func-continuation-arg expr)
                             (coerce (func-arglist expr) 'list)))
         (new-body (collect-closure-vars (func-body expr)
                                         inner-env
                                         inner-locals))
         (closure-arg (make-instance 'variable
                                     :name (make-gensym 'closure-env)
                                     :type :closure-env))
         (new-body-with-locals-bound
           (access-all-closure-vars new-body inner-env closure-arg))
         (new-locals (cons (func-name expr) local-vars))
         (new-in (collect-closure-vars (func-in expr)
                                       closure-env
                                       new-locals))
         (new-func (shallow-copy expr
                                 :closure-arg closure-arg
                                 :body new-body-with-locals-bound
                                 :closes-over inner-env
                                 :in new-in)))
    (maybe-add-many closure-env inner-env local-vars)
    new-func))

(defmethod collect-closure-vars ((expr if) &optional closure-env local-vars)
  (let* ((new-then (collect-closure-vars (if-then-clause expr)
                                              closure-env
                                              local-vars))
         (new-else (collect-closure-vars (if-else-clause expr)
                                         closure-env
                                         local-vars)))
    (maybe-add-var (if-predicate expr) closure-env local-vars)
    (shallow-copy expr
                  :then-clause new-then
                  :else-clause new-else)))

(defmethod collect-closure-vars ((expr apply) &optional closure-env local-vars)
  (maybe-add-var (apply-func expr) closure-env local-vars)
  (maybe-add-many closure-env (apply-args expr) local-vars)
  (maybe-add-var (apply-continuation expr) closure-env local-vars)
  expr)

(defmethod collect-closure-vars ((expr throw) &optional closure-env local-vars)
  (maybe-add-var (throw-cont expr) closure-env local-vars)
  (maybe-add-var (throw-arg expr) closure-env local-vars)
  expr)

(defun make-closures-explicit (program)
  (let* ((local-vars (list *exit-continuation*))
         (closure-env (make-adjustable-vector :element-type variable))
         (new-program (collect-closure-vars program closure-env local-vars)))
    (assert (uiop:emptyp closure-env))
    new-program))
