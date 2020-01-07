(uiop:define-package :hindley-milner/monomorphize
    (:mix
     :hindley-milner/typecheck/type
     :hindley-milner/typecheck/typed-ir1
     :iterate
     :trivial-types
     :cl)
  (:import-from :hindley-milner/typecheck/unify
   :unify)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :hindley-milner/typecheck/substitute
                :apply-substitution))
(cl:in-package :hindley-milner/monomorphize)

(gefjon-utils:defstruct globals
  ((polymorphic-values (association-list symbol expr))
   (monomorphic-values (association-list symbol expr))
   (existing-monomorphizations (association-list symbol (association-list type symbol)))))

(defun make-empty-globals ()
  (make-globals () () ()))

(defun push-poly-obj (globals let)
  (push (cons (let-binding let)
              (let-initform let))
        (globals-polymorphic-values globals))
  (values))

(defun collect-polymorphic-globals (program)
  "returns (`VALUES' GLOBALS ENTRY)

where GLOBALS is a `GLOBALS' object whose `POLYMORPHIC' records have been populated, and
ENTRY is an `EXPR' other than a `LET' where PROGRAM will begin execution"
  (iter
    (with globals = (make-empty-globals))
    (with top-level-expr = program)
    (unless (typep top-level-expr 'let)
      (return (values globals top-level-expr)))
    (push-poly-obj globals top-level-expr)
    (setf top-level-expr (let-body top-level-expr))))

(defun find-poly-obj-or-error (globals poly-name)
  (cdr (or (assoc poly-name (globals-polymorphic-values globals))
           (error "polymorphic value ~s not found" poly-name))))

(defun find-or-push-existing-monomorph-poly-cell (globals poly-name)
  (or (assoc poly-name (globals-existing-monomorphizations globals))
      (first (push (cons poly-name nil) (globals-existing-monomorphizations globals)))))

(defun insert-into-existing-monomorphizations-map (globals poly-name mono-type mono-name)
  (let* ((cell-for-this-poly (find-or-push-existing-monomorph-poly-cell globals poly-name))
         (cell-for-this-mono (cons mono-type mono-name)))
    (push cell-for-this-mono (cdr cell-for-this-poly)))
  (values))

(defun find-existing-monomorphization (globals poly-name mono-type)
  (cdr (assoc mono-type
              (cdr (assoc poly-name (globals-existing-monomorphizations globals)))
              :test #'equalp)))

(defun add-new-monomorphization (globals poly-name mono-type)
  (let* ((poly-expr (find-poly-obj-or-error globals poly-name))
         (poly-type (expr-type poly-expr))
         (substitution (unify mono-type poly-type))
         (mono-expr (apply-substitution substitution poly-expr))
         (mono-name (gensym (format nil "~s-~a" poly-name mono-type)))
         (monomorphic-value-cell (cons mono-name mono-expr)))
    (push monomorphic-value-cell
          (globals-monomorphic-values globals))
    (insert-into-existing-monomorphizations-map globals poly-name mono-type mono-name)
    mono-name))

(defgeneric monomorphize (expr globals))

(defmethod monomorphize ((var variable) globals)
  (make-variable
   (variable-type var)
   (or (find-existing-monomorphization globals (variable-name var) (variable-type var))
       (add-new-monomorphization globals (variable-name var) (variable-type var)))))

(defmethod monomorphize ((quote quote) globals)
  (declare (ignorable globals))
  quote)

(defmacro define-monomorphize (class &body body)
  (with-gensyms (thing globals)
    `(defmethod monomorphize ((,class ,class) ,globals)
       (flet ((recurse (,thing)
                (monomorphize ,thing ,globals)))
         ,@body))))

(define-monomorphize funcall
    (make-funcall (funcall-type funcall)
                  (recurse (funcall-function funcall))
                  (recurse (funcall-arg funcall))))

(define-monomorphize lambda
  (make-lambda (lambda-type lambda)
               (lambda-binding lambda)
               (recurse (lambda-body lambda))))

;; todo: properly handle polymorphic non-top-level lets
(define-monomorphize let
  (make-let (let-type let)
            (let-binding let)
            (let-scheme let)
            (recurse (let-initform let))
            (recurse (let-body let))))

(define-monomorphize if
  (make-if (if-type if)
           (recurse (if-predicate if))
           (recurse (if-then-case if))
           (recurse (if-else-case if))))

(define-monomorphize binop
  (make-binop (binop-type binop)
              (binop-op binop)
              (recurse (binop-lhs binop))
              (recurse (binop-rhs binop))))

(define-monomorphize prog2
  (make-prog2 (prog2-type prog2)
              (recurse (prog2-side-effect prog2))
              (recurse (prog2-return-value prog2))))

(defun monomorphize-program (program)
  "returns (`VALUES' ENTRY GLOBALS)

where ENTRY is a `TYPED-IR1:EXPR' and
GLOBALS is a `GLOBALS' with all its fields populated"
  (multiple-value-bind (globals entry) (collect-polymorphic-globals program)
    (cl:let ((monomorphic (monomorphize entry globals)))
      (values monomorphic globals))))
