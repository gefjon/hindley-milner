(uiop:define-package :hindley-milner/typecheck
    (:nicknames :typecheck)
  (:use :cl)
  (:import-from :hindley-milner/typecheck/infer))
(cl:in-package :hindley-milner/typecheck)

;; ;;;; types
;; ;; in the hindley-milner type system, `TYPE' refers to the type analog
;; ;; of the expression language. `lambda' bindings (as opposed to
;; ;; `let'-bindings) take on these types, which are never
;; ;; polymorphic. `let'-bindings (which includes top-level `def's as
;; ;; well) instead have `TYPE-SCHEME's, which are polymorphic.

;; (deftype type-variable ()
;;   'symbol)

;; (defun new-type-variabe (&optional (name "type-variable-"))
;;   (alexandria:make-gensym name))

;; (defenum type
;;   type-variable
;;   (-> ((inputs (proper-list type))
;;        (output type))))

;; ;;;; type-schemes
;; ;;; `let'-bindings and top-level `def's take on polymorphic types

;; (gefjon-utils:defstruct type-scheme
;;   ((bindings (proper-list type-variable))
;;    (body type)))

;; (declaim (ftype (function (type-scheme type-variable) boolean)
;;                 type-scheme-closes-over-p))
;; (defun type-scheme-closes-over-p (scheme variable)
;;   (not (not (member variable (type-scheme-bindings scheme)))))

;; ;; this struct exists (as opposed to just using an alist) to enable
;; ;; generic function specializaiton
;; (gefjon-utils:defstruct type-env
;;   ((alist (association-list type-variable type-scheme))))

;; (declaim (ftype (function (type-env type-variable type-scheme) type-env)
;;                 extend))
;; (defun extend (env var scheme)
;;   (make-type-env (acons var scheme (type-env-alist env))))

;; (declaim (ftype (function (type-env type-variable) type-scheme)
;;                 type-env-lookup))
;; (defun type-env-lookup (env var)
;;   (let ((assoc (assoc var (type-env-alist env))))
;;     (unless assoc (error "unbound type-variable ~s in ~s" var env))
;;     (cdr assoc)))

;; ;;;; substitution

;; (deftype substitution ()
;;   ;; unlike `TYPE-ENV', we don't have to do dispatch on subsitutions
;;   '(association-list type-variable type))

;; (declaim (ftype (function (type-variable type) substitution)
;;                 singleton-subtitution))
;; (defun singleton-subtitution (type-variable type)
;;   (acons type-variable type ()))

;; (declaim (ftype (function (substitution &rest substitution) substitution)
;;                 compose))
;; (defun compose (enclosing &rest inner)
;;   (apply #'concatenate 'list enclosing inner))

;; (defgeneric apply-substitution (substitution target))

;; ;;; implementations for HM:TYPE
;; (defmethod apply-substitution (substitution (target symbol))
;;   "if TARGET is bound in SUBSTITUTION, use its type; otherwise, return TARGET"
;;   (or (cdr (assoc target substitution)) target))

;; (defmethod apply-substitution (substitution (target ->))
;;   (flet ((recurse (on) (apply-substitution substitution on)))
;;     (make--> (recurse (->-inputs target))
;;              (recurse (->-output target)))))

;; ;;; implementations for TYPE-SCHEME
;; (defmethod apply-substitution (substitution (target type-scheme))
;;   (flet ((closed-over-p (type-var)
;;            (type-scheme-closes-over-p target type-var)))
;;     (make-type-scheme (type-scheme-bindings target) (apply-substitution (remove-if #'closed-over-p substitution
;;                                                                                    :key #'car)
;;                                                                         (type-scheme-body target)))))

;; ;;; implementations for lists
;; (defmethod apply-substitution (substitution (target cons))
;;   (iter (for el in target)
;;     (collecting (apply-substitution substitution el))))

;; (defmethod apply-substitution (substitution (target null))
;;   (declare (ignorable substitution target))
;;   ())

;; ;;; implementation for TYPE-ENV
;; (defmethod apply-substitution (substitution (target type-env))
;;   (flet ((substitute-alist-cell (cell)
;;            (destructuring-bind (key . value) cell
;;              (cons key (apply-substitution substitution value)))))
;;     (make-type-env (mapcar #'substitute-alist-cell (type-env-alist target)))))

;; (defgeneric free-type-variables (within))

;; ;;; implementations for HM:TYPE
;; (defmethod free-type-variables ((within symbol))
;;   (list within))

;; (defmethod free-type-variables ((within ->))
;;   (union (free-type-variables (->-input within))
;;          (free-type-variables (->-output within))))

;; ;;; implementations for TYPE-SCHEME
;; (defmethod free-type-variables ((within type-scheme))
;;   (flet ((closed-over-p (type-var)
;;            (type-scheme-closes-over-p within type-var)))
;;     (remove-if #'closed-over-p (free-type-variables (type-scheme-body within)))))

;; ;;; implementations for lists
;; (defmethod free-type-variables ((within cons))
;;   (iter (for el in within)
;;     (unioning (free-type-variables el))))

;; (defmethod free-type-variables ((within null))
;;   (declare (ignorable within))
;;   ())

;; ;;; implementation for TYPE-ENV
;; (defmethod free-type-variables ((within type-env))
;;   (iter (for (key . value) in (type-env-alist within))
;;     (unioning (free-type-variables value))))

;; ;;;; instantiate/generalize
;; (declaim (ftype (function (type-scheme) type)
;;                 instantiate))
;; (defun instantiate (scheme)
;;   (flet ((substitute-plist-cell (type-var) (cons type-var
;;                                                  (new-type-variable type-var))))
;;     (apply-substitution (mapcar #'substitute-plist-cell (type-scheme-bindings scheme))
;;                  (type-scheme-body scheme))))

;; (declaim (ftype (function (type &optional type-env) type-scheme)
;;                 generalize))
;; (defun generalize (type &optional (env *empty-type-env*))
;;   (make-type-scheme (set-difference (free-type-variables type)
;;                                     (free-type-variables env))
;;                     type))

;; (gefjon-utils:defstruct constraint
;;   ((lhs type)
;;    (rhs type)))

;; (defmethod apply-substitution (substitution (target constraint))
;;   (with-slots (lhs rhs) target
;;     (make-constraint (apply-substitution substitution lhs)
;;                      (apply-substitution substitution rhs))))

;; (defgeneric infer (expr type-env)
;;   (:documentation "returns (VALUES TYPE CONSTRAINTS), where TYPE is the type of EXPR and CONSTRAINTS is a list of unifying constraints"))

;; (defmethod infer ((expr symbol) type-env)
;;   (values (instantiate (type-env-lookup type-env expr))
;;           ()))

;; (defmethod infer ((expr syntax:lambda) type-env)
;;   (multiple-value-bind (ftype fconstraints) (infer (syntax:funcall-function expr)
;;                                                    type-env)
;;     ))
