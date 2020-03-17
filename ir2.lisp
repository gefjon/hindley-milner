(uiop:define-package :hindley-milner/ir2
    (:mix
     :iterate
     :hindley-milner/prologue
     :trivial-types
     :cl)
  (:nicknames :ir2)
  (:use-reexport
   :hindley-milner/ir2/repr-type
   :hindley-milner/ir2/place
   :hindley-milner/ir2/instr
   :hindley-milner/ir2/procedure
   :hindley-milner/ir2/program)
  (:import-from :alexandria :make-gensym)
  (:import-from :hindley-milner/monomorphize
   :mono-let :mono-let-binding :mono-let-bound-type :mono-let-initform :mono-let-body)
  (:shadowing-import-from :generic-cl
   :make-hash-map :get)
  (:import-from :hindley-milner/syntax :operator)
  (:shadowing-import-from :gefjon-utils
   :|:| :-> :optional)
  (:export
   :transform-program))
(cl:in-package :hindley-milner/ir2)

(defvar *program*)
(defvar *current-procedure*)

(defmacro instr (class-name &rest args)
  `(push-instr *current-procedure*
               (make-instance ',class-name
                              ,@args)))

(|:| #'find-variable (-> (symbol) place))
(defun find-variable (name)
  (or (find-arg name *current-procedure*)
      (find-local name *current-procedure*)
      (find-global name *program*)
      (error "unknown variable ~a" name)))

(|:| #'place-for-let (-> (mono-let &optional place-variety) place))
(defun place-for-let (mono-let &optional (place-type 'local))
  (make-instance place-type
                 :name (mono-let-binding mono-let)
                 :type (repr-for-ir1-type (mono-let-bound-type mono-let))))

(|:| #'new-local (-> (&key
                      (:place (optional local))
                      (:type (optional repr-type))
                      (:name (optional symbol)))
                     local))
(defun new-local (&key place type name)
  (unless (or place type)
    (error "must provide either a PLACE or a TYPE to NEW-LOCAL"))
  (let ((place (or place (make-instance 'local
                                        :name (or name (gensym))
                                        :type type))))
    
    (vector-push-extend place (procedure-locals *current-procedure*))
    place))

(|:| #'add-global-place (-> (mono-let) global))
(defun add-global-place (mono-let)
  "returns a PLACE"
  (let ((place (place-for-let mono-let 'global)))
    (vector-push-extend place (program-globals *program*))
    place))

(defgeneric convert-expr (ir1-expr &key bound-name &allow-other-keys)
  (:documentation
   "convert IR1-EXPR into a series of INSTRs in *CURRENT-PROCEDURE*

if BOUND-NAME is supplied, it should be a symbol to which the result
of IR1-EXPR will be bound. this is used to produce better debug
info."))

;; (LET (foo bar) baz) => bar; SET foo; baz
(defmethod convert-expr ((expr mono-let) &rest args &key &allow-other-keys)
  (let ((place (place-for-let expr)))
    (new-local :place place)
    (convert-expr (mono-let-initform expr) :bound-name (mono-let-binding expr))
    (instr set-var :dest place))
  (apply #'convert-expr (mono-let-body expr) args)
  (values))

;; (VARIABLE foo) => GET foo
(defmethod convert-expr ((expr ir1:variable) &key &allow-other-keys)
  (let* ((name (ir1:variable-name expr))
         (place (find-variable name)))
    (instr get-var :src place)))

;; (QUOTE foo) => CONST repr-type foo
(defmethod convert-expr ((expr ir1:quote) &key &allow-other-keys)
  (instr const
         :type (repr-for-ir1-type (ir1:expr-type expr))
         :value (ir1:quote-it expr)))

(|:| #'funcall-arg-type (-> (ir1:funcall) ir1-type:type))
(defun funcall-arg-type (funcall)
  (ir1-type:->-input (ir1:expr-type (ir1:funcall-function funcall))))

;; (FUNCALL (FUNCALL func arg1) arg2) => arg1; arg2; func; CALL ftype
(defmethod convert-expr ((expr ir1:funcall) &key &allow-other-keys)
  (flet ((convert-arg-returning-type (funcall)
           (let* ((type (repr-for-ir1-type (funcall-arg-type funcall))))
             (convert-expr (ir1:funcall-arg funcall))
             type))
         (convert-func (expr ftype)
           (convert-expr expr)
           (instr call :function-type ftype)))
    (iter
      (for funcall first expr then (ir1:funcall-function funcall))
      (while (typep funcall 'ir1:funcall))
      (collect (convert-arg-returning-type funcall)
        into arg-types
        at end
        result-type (vector repr-type))
      (finally
       (let* ((result-type (repr-for-ir1-type (ir1:expr-type expr)))
              (ftype (make-instance 'function-type
                                    :inputs arg-types
                                    :result result-type)))
         (convert-func funcall ftype)))))
  (values))

(|:| #'lambda-arg-place (-> (ir1:lambda) argument))
(defun lambda-arg-place (lambda)
  (make-instance 'argument
                 :name (ir1:lambda-binding lambda)
                 :type (repr-for-ir1-type (ir1-type:->-input (ir1:expr-type lambda)))))

;; (LAMBDA arg1 (LAMBDA arg2 body)) => FUNCTION-POINTER gensym
(defmethod convert-expr ((expr ir1:lambda) &key (bound-name 'lambda) &allow-other-keys)
  (flet ((make-proc (args body)
           ;; returns the new proc's symbol name
           (let* ((proc-name (make-gensym bound-name))
                  (->-type (ir1:expr-type expr))
                  (*current-procedure*
                    (make-empty-procedure proc-name args ->-type)))
             (convert-expr body)
             (instr ret :type (repr-for-ir1-type (ir1:expr-type body)))
             (add-procedure *program* *current-procedure*)
             proc-name)))
    (iter
      (for lambda first expr then (ir1:lambda-body lambda))
      (while (typep lambda 'ir1:lambda))
      (for arg = (lambda-arg-place lambda))
      (collect arg into args at end)
      (finally
       (instr func-pointer :name (make-proc args lambda))))))

;; (IF pred then else) =>
;;   pred;
;;   GOTO-IF then-lbl;
;;   else;
;;   GOTO end-lbl;
;;   LABEL then-lbl;
;;   then;
;;   LABEL end-lbl;
(defmethod convert-expr ((expr ir1:if) &rest args &key &allow-other-keys)
  (let* ((then-label (gensym "then-case"))
         (end-label (gensym "end")))
    (convert-expr (ir1:if-predicate expr))
    (instr goto-if :target then-label)
    ;; compile the else-case first to avoid an extra branch
    (apply #'convert-expr (ir1:if-else-case expr) args)
    (instr goto :target end-label)
    (instr label :name then-label)
    (apply #'convert-expr (ir1:if-then-case expr) args)
    (instr label :name end-label)))

(defmethod convert-expr ((expr ir1:binop) &key &allow-other-keys)
  (convert-expr (ir1:binop-lhs expr))
  (convert-expr (ir1:binop-rhs expr))
  (instr binop :op (ir1:binop-op expr)))

(|:| #'convert-and-discard (-> (ir1:expr) (values &optional)))
(defun convert-and-discard (ir1-expr)
  (convert-expr ir1-expr)
  (instr drop))

(defmethod convert-expr ((expr ir1:prog2) &rest args &key &allow-other-keys)
  (convert-and-discard (ir1:prog2-side-effect expr))
  (apply #'convert-expr (ir1:prog2-return-value expr) args))

(|:| #'collect-globals (-> (ir1:expr) ir1:expr))
(defun collect-globals (ir1-prog)
  "returns the first IR1:EXPR in IR1-PROG that is not a MONO-LET"
  (if (typep ir1-prog 'monomorphize:mono-let)
      (progn
        (convert-expr (mono-let-initform ir1-prog)
                      :bound-name (mono-let-binding ir1-prog))
        (instr set-var :dest (add-global-place ir1-prog))
        (collect-globals (mono-let-body ir1-prog)))
      ir1-prog))

(|:| #'transform-program (-> (ir1:expr) program))
(defun transform-program (ir1-prog)
  (multiple-value-bind (*program* *current-procedure*) (make-empty-program)
    (let* ((entry-after-globals (collect-globals ir1-prog)))
      (convert-and-discard entry-after-globals))
    *program*))
