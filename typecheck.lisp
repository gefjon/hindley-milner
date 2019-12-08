(uiop:define-package :hindley-milner/typecheck
    (:nicknames :typecheck)
  (:use :cl)
  (:import-from :hindley-milner/typecheck/substitute
                :substitution :apply-substitution)
  (:import-from :hindley-milner/typecheck/infer)
  (:import-from :hindley-milner/typecheck/unify)
  (:export :infer-program-types :*top-level-type-env*))
(cl:in-package :hindley-milner/typecheck)

(defgeneric substitute-node (substitution target)
  (:method-combination progn))

(defmethod substitute-node progn (substitution (target ir1:typed-node))
  (when (ir1:type-already-computed-p target)
    (setf (ir1:typed-node-type target)
          (apply-substitution substitution (ir1:typed-node-type target))))
  (values))

(defmacro def-node-substitution (class &rest accessors)
  (flet ((recurse (accessor)
           `(substitute-node substitution (,accessor target))))
    `(defmethod substitute-node progn (substitution (target ,class))
       ,@(mapcar #'recurse accessors))))
(defmacro node-substitutions (&rest clauses)
  (flet ((do-def (clause)
           `(def-node-substitution ,@clause)))
    `(progn ,@(mapcar #'do-def clauses))))

(node-substitutions
 (ir1:funcall ir1:funcall-arg ir1:funcall-function)
 (ir1:lambda ir1:lambda-body)
 (ir1:let ir1:let-initform ir1:let-body)
 (ir1:if ir1:if-predicate ir1:if-then-case ir1:if-else-case)
 (ir1:binop ir1:binop-lhs ir1:binop-rhs)
 (ir1:prog2 ir1:prog2-side-effect ir1:prog2-return-value))

(defvar *top-level-type-env* '())

(defun infer-program-types (program)
  "annotate a program, expressed as an `IR1:EXPR', with inferred type information"
  (multiple-value-bind (return-type constraints) (infer:infer program *top-level-type-env*)
    (declare (ignore return-type))
    (let ((substitution (unify:solve constraints)))
      (substitute-node substitution program)))
  (values))
