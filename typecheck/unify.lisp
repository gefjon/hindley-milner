(uiop:define-package :hindley-milner/typecheck/unify
    (:nicknames :unify)
  (:mix :hindley-milner/ir1/type :iterate :cl)
  (:import-from :hindley-milner/typecheck/infer
   :constraints :constraint :constraint-lhs :constraint-rhs)
  (:import-from :hindley-milner/typecheck/substitute
   :substitution :apply-substitution)
  (:export :solve :unify))
(cl:in-package :hindley-milner/typecheck/unify)

(declaim (ftype (function (constraints) substitution)
                solve))
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

(declaim (ftype (function (constraint) (values substitution &optional))
                solve-constraint))
(defun solve-constraint (constraint)
  (unify (constraint-lhs constraint) (constraint-rhs constraint)))

(defgeneric unify (lhs rhs)
  (:documentation "returns a unifying `SUBSTITUTION' for the constraint that LHS and RHS are the same type"))

(declaim (ftype (function (type-variable type) (values substitution &optional))
                bind))
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

(defmethod unify ((lhs ->) (rhs ->))
  (iter
    (for lhs-input in-vector (->-inputs lhs))
    (for rhs-input in-vector (->-inputs rhs))
    (collect (cons lhs-input rhs-input) into input-constraints at beginning)
    (finally
     (return
       (solve (acons (->-output lhs)
                     (->-output rhs)
                     input-constraints))))))

(defmethod unify ((lhs type-primitive) (rhs type-primitive))
  (unless (eq (type-primitive-name lhs) (type-primitive-name rhs))
    (error "cannot unify type-primitives ~s with ~s"
           (type-primitive-name lhs)
           (type-primitive-name rhs))))
