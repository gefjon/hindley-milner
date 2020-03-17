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

(|:| #'find-variable (-> (symbol program procedure) place))
(defun find-variable (name program procedure)
  (or (find-arg name procedure)
      (find-local name procedure)
      (find-global name program)
      (error "unknown variable ~a" name)))

(|:| #'place-for-let (-> (mono-let &optional place-variety) place))
(defun place-for-let (mono-let &optional (place-type 'local))
  (make-instance place-type
                 :name (mono-let-binding mono-let)
                 :type (repr-for-ir1-type (mono-let-bound-type mono-let))))

(|:| #'new-local (-> (procedure &key
                                (:place (optional local))
                                (:type (optional repr-type))
                                (:name (optional symbol)))
                     local))
(defun new-local (procedure &key place type name)
  (unless (or place type)
    (error "must provide either a PLACE or a TYPE to NEW-LOCAL"))
  (let ((place (or place (make-instance 'local
                                        :name (or name (gensym))
                                        :type type))))
    
    (vector-push-extend place (procedure-locals procedure))
    place))

(|:| #'add-global-place (-> (program mono-let) global))
(defun add-global-place (program mono-let)
  "returns a PLACE"
  (let ((place (place-for-let mono-let 'global)))
    (vector-push-extend place (program-globals program))
    place))

(defgeneric convert-expr (ir1-expr &key program procedure)
  (:documentation
   "convert IR1-EXPR into a series of INSTRs in PROCEDURE which store their result into STORE-INTO"))

;; (LET (foo bar) baz) => bar; SET foo; baz
(defmethod convert-expr ((expr mono-let) &key program procedure)
  (let ((place (place-for-let expr)))
    (new-local procedure :place place)
    (convert-expr (mono-let-initform expr)
                  :program program
                  :procedure procedure)
    (push-instr procedure
                (make-instance 'set-var
                               :dest place)))
  (convert-expr (mono-let-body expr)
                :program program
                :procedure procedure)
  (values))

;; (VARIABLE foo) => GET foo
(defmethod convert-expr ((expr ir1:variable) &key program procedure)
  (let* ((name (ir1:variable-name expr))
         (place (find-variable name program procedure))
         (mov (make-instance 'get-var
                             :src place)))
    (push-instr procedure mov)))

;; (QUOTE foo) => CONST repr-type foo
(defmethod convert-expr ((expr ir1:quote) &key program procedure)
  (declare (ignore program))
  (push-instr procedure
              (make-instance 'const
                  :type (repr-for-ir1-type (ir1:expr-type expr))
                  :value (ir1:quote-it expr))))

(|:| #'funcall-arg-type (-> (ir1:funcall) ir1-type:type))
(defun funcall-arg-type (funcall)
  (ir1-type:->-input (ir1:expr-type (ir1:funcall-function funcall))))

;; (FUNCALL (FUNCALL func arg1) arg2) => arg1; arg2; func; CALL ftype
(defmethod convert-expr ((expr ir1:funcall) &key program procedure)
  (flet ((convert-arg-returning-type (funcall)
           (let* ((type (repr-for-ir1-type (funcall-arg-type funcall))))
             (convert-expr (ir1:funcall-arg funcall)
                           :program program
                           :procedure procedure)
             type))
         (convert-func (expr ftype)
           (let* ((call (make-instance 'call
                                       :function-type ftype)))
             (convert-expr expr
                           :program program
                           :procedure procedure)
             (push-instr procedure call))))
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
(defmethod convert-expr ((expr ir1:lambda) &key program ((:procedure caller)))
  (flet ((make-proc (args body)
           ;; returns the new proc's symbol name
           (let* ((proc-name (make-gensym "lambda"))
                  (->-type (ir1:expr-type expr))
                  (new-proc (make-empty-procedure proc-name args ->-type))
                  (ret (make-instance 'ret
                                      :type (repr-for-ir1-type (ir1:expr-type body)))))
             (convert-expr body
                           :procedure new-proc
                           :program program)
             (push-instr new-proc ret)
             (add-procedure program new-proc)
             proc-name)))
    (iter
      (for lambda first expr then (ir1:lambda-body lambda))
      (while (typep lambda 'ir1:lambda))
      (for arg = (lambda-arg-place lambda))
      (collect arg into args at end)
      (finally
       (push-instr caller
                   (make-instance 'func-pointer
                                  :name (make-proc args lambda)))))))

;; (IF pred then else) =>
;;   pred;
;;   GOTO-IF then-lbl;
;;   else;
;;   GOTO end-lbl;
;;   LABEL then-lbl;
;;   then;
;;   LABEL end-lbl;
(defmethod convert-expr ((expr ir1:if) &key program procedure)
  (let* ((then-label-name (gensym "then-case"))
         (then-label (make-instance 'label :name then-label-name))
         (end-label-name (gensym "end"))
         (end-label (make-instance 'label :name end-label-name))
         (cond-branch (make-instance 'goto-if
                                     :target then-label-name))
         (end-branch (make-instance 'goto
                                    :target end-label-name)))
    (convert-expr (ir1:if-predicate expr)
                  :program program
                  :procedure procedure)
    (push-instr procedure cond-branch)
    ;; compile the else-case first to avoid an extra branch
    (convert-expr (ir1:if-else-case expr)
                  :program program
                  :procedure procedure)
    (push-instr procedure end-branch)
    (push-instr procedure then-label)
    (convert-expr (ir1:if-then-case expr)
                  :program program
                  :procedure procedure)
    (push-instr procedure end-label)))

(defmethod convert-expr ((expr ir1:binop) &key program procedure)
  (let* ((lhs-expr (ir1:binop-lhs expr))
         (rhs-expr (ir1:binop-rhs expr))
         (binop (make-instance 'binop
                               :op (ir1:binop-op expr))))
    (convert-expr lhs-expr
                  :program program
                  :procedure procedure)
    (convert-expr rhs-expr
                  :program program
                  :procedure procedure)
    (push-instr procedure binop)))

(|:| #'convert-and-discard (-> (ir1:expr program procedure) (values &optional)))
(defun convert-and-discard (ir1-expr program procedure)
  (convert-expr ir1-expr
                :program program
                :procedure procedure)
  (push-instr procedure
              (make-instance 'drop)))

(defmethod convert-expr ((expr ir1:prog2) &key program procedure)
  (convert-and-discard (ir1:prog2-side-effect expr) program procedure)
  (convert-expr (ir1:prog2-return-value expr)
                :program program
                :procedure procedure))

(|:| #'collect-globals (-> (ir1:expr program procedure) ir1:expr))
(defun collect-globals (ir1-prog program entry)
  "returns the first IR1:EXPR in IR1-PROG that is not a MONO-LET"
  (if (typep ir1-prog 'monomorphize:mono-let)
      (progn
        (convert-expr (mono-let-initform ir1-prog)
                      :program program
                      :procedure entry)
        (push-instr entry
                    (make-instance 'set-var
                                   :dest (add-global-place program ir1-prog)))
        (collect-globals (mono-let-body ir1-prog) program entry))
      ir1-prog))

(|:| #'transform-program (-> (ir1:expr) program))
(defun transform-program (ir1-prog)
  (multiple-value-bind (program entry) (make-empty-program)
    (let* ((entry-after-globals (collect-globals ir1-prog program entry)))
      (convert-and-discard entry-after-globals program entry))
    program))
