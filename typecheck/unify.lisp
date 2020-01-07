(uiop:define-package :hindley-milner/typecheck/unify
    (:nicknames :unify)
  (:mix :hindley-milner/typecheck/type :iterate :cl)
  (:import-from :hindley-milner/typecheck/infer
   :constraints :constraint :constraint-lhs :constraint-rhs :constraint-acons)
  (:import-from :hindley-milner/typecheck/substitute
   :substitution :apply-substitution)
  (:export :solve :unify))
(cl:in-package :hindley-milner/typecheck/unify)

(declaim (ftype (function (constraints) substitution)
                solve))
(defun solve (constraints)
  (labels ((recursive-solve (constraints partial-subst)
           (if (null constraints)
               (return-from solve partial-subst)
               (let* ((constraint (first constraints))
                      (new-subst (solve-constraint constraint))
                      (remaining-constraints (apply-substitution new-subst (rest constraints)))
                      (partial-solution (append new-subst partial-subst)))
                 (recursive-solve remaining-constraints partial-solution)))))
    (recursive-solve constraints '())))

(declaim (ftype (function (constraint) substitution)
                solve-constraint))
(defun solve-constraint (constraint)
  (unify (constraint-lhs constraint) (constraint-rhs constraint)))

(defgeneric unify (lhs rhs)
  (:documentation "returns a unifying `SUBSTITUTION'"))

(declaim (ftype (function (symbol type) substitution)
                bind))
(defun bind (tvar type)
  (acons tvar type '()))

(defmethod unify ((lhs symbol) rhs)
  (unless (eq lhs rhs)
    (bind lhs rhs)))

(defmethod unify (lhs (rhs symbol))
  (unless (eq rhs lhs)
    (bind rhs lhs)))

(defmethod unify ((lhs ->) (rhs ->))
  (solve (constraint-acons (->-input lhs)
                           (->-input rhs)
                           (constraint-acons (->-output lhs)
                                             (->-output rhs)
                                             ()))))

(defmethod unify ((lhs type-primitive) (rhs type-primitive))
  (unless (equalp lhs rhs)
    (error "cannot unify ~s with ~s" lhs rhs)))
