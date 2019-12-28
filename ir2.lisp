(uiop:define-package :hindley-milner/ir2
    (:use :hindley-milner/defenum :trivial-types :cl)
  (:shadowing-import-from :hindley-milner/typecheck/type
                          :type)
  (:nicknames :ir2)
  (:import-from :hindley-milner/ir1))
(cl:in-package :hindley-milner/ir2)

(defenum expr ())

(gefjon-utils:defstruct procedure
  ((args (proper-list type))
   (locals (proper-list type))
   (return type)
   (body expr)))

(gefjon-utils:defstruct global
  ((type type)
   (initform expr)))

(deftype index ()
  '(unsigned-byte 32))

(defenum binding
    ((procedure-binding ((index index)))
     (global-binding ((index index)))
     (local-binding ((index index)))))

(deftype binding-env ()
  '(association-list symbol binding))

(gefjon-utils:defstruct program
  ((procedures (vector procedure))
   (globals (vector global))
   (global-env binding-env)))

(declaim (ftype (function (vector t) index)
                vector-push-return-index))
(defun vector-push-return-index (vector element)
  (let ((old-length (length vector)))
    (vector-push-extend element vector)
    old-length))

(defun transform-body-expr (ir1-expr env)
  (error "unimplemented"))

(defun process-entry-form (program form)
  (multiple-value-bind (expr locals) (transform-body-expr form (program-global-env program))
    (add-procedure (make-procedure () locals type:*void* expr) 'main program))
  (values))

(declaim (ftype (cl:function (program ir1:expr) program)
                recursively-process-top-level-bindings))
(defun recurisvely-process-top-level-bindings (program form)
  (if (typep form 'ir1:let)
      (corecursively-add-top-level-binding program form)
      (process-entry-form program form)))

(declaim (ftype (cl:function (program ir1:let) program)
                corecursively-add-top-level-binding))
(defun corecursively-add-top-level-binding (program let)
  (if (typep (ir1:let-body let) 'ir1:lambda)
      (add-top-level-function program
                              (ir1:let-binding let)
                              (ir1:let-initform let)
                              (ir1:let-scheme let))
      (add-global program
                  (ir1:let-binding let)
                  (ir1:let-initform let)
                  (ir1:let-scheme let)))
  (recursively-process-top-level-bindings program (ir1:let-body let)))

(defun add-procedure (procedure name program)
  (let* ((index (vector-push-return-index (program-procedures program) procedure))
         (binding (make-procedure-binding index)))
    (push (cons name binding) (program-global-env program)))
  (values))

(defun add-top-level-function (program name lambda type-scheme)
  (add-procedure (build-procedure-from-lambda lambda type-scheme (program-global-env program)))
  (values))

(defun add-global (program name initform type-scheme)
  (let* ((global (build-global initform type-scheme (program-global-env program)))
         (index (vector-push-return-index (program-globals program) global))
         (binding (make-global-binding index)))
    (push (cons name binding) (program-global-env program)))
  (values))

(defun make-specialized-vector (element-type)
  (make-array '(0) :element-type element-type
                   :adjustable t
                   :fill-pointer 0))
(defun make-empty-program ()
  (make-program (make-specialized-vector 'procedure)
                (make-specialized-vector 'global)
                ()))

(declaim (ftype (function (ir1:expr) program)
                parse-program))
(defun parse-program (ir1-program)
  (let ((program (make-empty-program)))
    (recursively-process-top-level-bindings program ir1-program)
    program))
