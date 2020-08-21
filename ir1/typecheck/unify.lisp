(uiop:define-package :hindley-milner/ir1/typecheck/unify
  (:mix :hindley-milner/prologue :hindley-milner/ir1/type :iterate :cl)
  (:import-from :hindley-milner/ir1/typecheck/infer
   :constraints :constraint :lhs :rhs)
  (:import-from :hindley-milner/ir1/typecheck/substitute
   :substitution :apply-substitution)
  (:export :solve :unify))
(cl:in-package :hindley-milner/ir1/typecheck/unify)

(|:| #'solve (-> (constraints) substitution))
(defun solve (constraints)
  "returns a unifying `SUBSTITUTION' for CONSTRAINTS."
  (labels ((recursive-solve (constraints partial-subst)
           (if (null constraints)
               (return-from solve partial-subst)
               (let* ((constraint (first constraints))
                      (new-subst (solve-constraint constraint))
                      (remaining-constraints (apply-substitution new-subst (rest constraints)))
                      (partial-solution (append new-subst partial-subst)))
                 (recursive-solve remaining-constraints partial-solution)))))
    (recursive-solve constraints '())))

(|:| #'solve-constraint (-> (constraint) substitution))
(defun solve-constraint (constraint)
  (unify (lhs constraint) (rhs constraint)))

(defgeneric unify (lhs rhs)
  (:documentation "returns a unifying `SUBSTITUTION' for the constraint that LHS and RHS are the same type"))

(|:| #'bind (-> (type-variable type) substitution))
(defun bind (tvar type)
  (acons tvar type '()))

(defmethod unify :around (lhs rhs)
  "avoid generating redundant constraints for types which are already `EQ'"
  (unless (eq lhs rhs)
    (call-next-method)))

(defmethod unify ((lhs type-variable) rhs)
  (bind lhs rhs))

(defmethod unify (lhs (rhs type-variable))
  (bind rhs lhs))

(defmethod unify ((lhs arrow) (rhs arrow))
  (iter
    (for lhs-input in-vector (inputs lhs))
    (for rhs-input in-vector (inputs rhs))
    (collect (cons lhs-input rhs-input) into input-constraints at beginning)
    (finally
     (return
       (solve (acons (output lhs)
                     (output rhs)
                     input-constraints))))))

(defmethod unify ((lhs type-primitive) (rhs type-primitive))
  (unless (eq (name lhs) (name rhs))
    (error "cannot unify type-primitives ~s with ~s"
           (name lhs)
           (name rhs))))
