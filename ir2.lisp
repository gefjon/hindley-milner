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

(defun place-for-let (mono-let &optional (place-type 'local))
  (make-instance place-type
                 :name (mono-let-binding mono-let)
                 :type (repr-for-ir1-type (mono-let-bound-type mono-let))))

(defun new-local (procedure &key place type name)
  (assert (or place type) (place type) "must provide either a place or type to NEW-LOCAL")
  (unless place
    (setf place (make-instance 'local
                               :name (or name (gensym))
                               :type type)))
  (vector-push-extend place (procedure-locals procedure))
  place)

(defun add-global-place (program mono-let)
  "returns a PLACE"
  (let ((place (place-for-let mono-let 'global)))
    (vector-push-extend place (program-globals program))
    place))

(defgeneric convert-expr (ir1-expr &key store-into program procedure)
  (:documentation
   "convert IR1-EXPR into a series of INSTRs in PROCEDURE which store their result into STORE-INTO"))

;; (LET (foo bar) baz) => (MOV foo bar) baz
(defmethod convert-expr ((expr mono-let) &key store-into program procedure)
  (let ((place (place-for-let expr)))
    (new-local procedure :place place)
    (convert-expr (mono-let-initform expr)
                  :store-into place
                  :program program
                  :procedure procedure))
  (convert-expr (mono-let-body expr)
                :store-into store-into
                :program program
                :procedure procedure)
  (values))

;; (VARIABLE foo) => (MOV dest foo)
(defmethod convert-expr ((expr ir1:variable) &key store-into program procedure)
  (let* ((name (ir1:variable-name expr))
         (mov (make-instance 'mov
                            :dest store-into
                            :src (find-variable name program procedure))))
    (push-instr procedure mov)))

;; (QUOTE foo) => (CONST dest foo)
(defmethod convert-expr ((expr ir1:quote) &key store-into program procedure)
  (declare (ignore program))
  (push-instr procedure
              (make-instance 'const
                  :dest store-into
                  :value (ir1:quote-it expr))))

(defun funcall-arg-type (funcall)
  (ir1-type:->-input (ir1:expr-type (ir1:funcall-function funcall))))

;; (FUNCALL (FUNCALL func arg1) arg2) => (PARAM arg1) (PARAM arg2) (CALL func)
(defmethod convert-expr ((expr ir1:funcall) &key store-into program procedure)
  (flet ((convert-arg (funcall)
           (let* ((place (new-local
                          procedure :type (repr-for-ir1-type (funcall-arg-type funcall))))
                  (param (make-instance 'param
                                        :src place)))
             (convert-expr (ir1:funcall-arg funcall)
                           :store-into place
                           :program program
                           :procedure procedure)
             (push-instr procedure param)))
         (convert-func (expr arity)
           (let* ((func-place (new-local procedure :type :code-pointer))
                  (call (make-instance 'call
                                       :dest store-into
                                       :procedure func-place
                                       :param-count arity)))
             (convert-expr expr
                           :store-into func-place
                           :program program
                           :procedure procedure)
             (push-instr procedure call))))
    (iter
      (for arity upfrom 1)
      (for funcall first expr then (ir1:funcall-function funcall))
      (while (typep funcall 'ir1:funcall))
      (convert-arg funcall)
      (finally
       (convert-func funcall arity))))
  (values))

(defun lambda-arg-place (lambda)
  (make-instance 'argument
                 :name (ir1:lambda-binding lambda)
                 :type (repr-for-ir1-type (ir1-type:->-input (ir1:expr-type lambda)))))

(defmethod convert-expr ((expr ir1:lambda) &key store-into program ((:procedure caller)))
  (flet ((make-proc (args body)
           ;; returns the new proc's symbol name
           (let* ((proc-name (make-gensym (place-name store-into)))
                  (new-proc (make-empty-procedure proc-name args))
                  (ret-place (new-local new-proc :type (repr-for-ir1-type (ir1:expr-type body))))
                  (ret (make-instance 'ret
                                      :val ret-place)))
             (convert-expr body
                           :store-into ret-place
                           :procedure new-proc
                           :program program)
             (push-instr new-proc ret)
             (add-procedure program new-proc))))
    (iter
      (for lambda first expr then (ir1:lambda-body lambda))
      (while (typep lambda 'ir1:lambda))
      (for arg = (lambda-arg-place lambda))
      (collect arg into args at end)
      (finally
       (push-instr caller
                   (make-instance 'func-pointer
                                  :dest store-into
                                  :func (make-proc args lambda)))))))

(defmethod convert-expr ((expr ir1:if) &key store-into program procedure)
  (let* ((pred-place (new-local procedure :type :boolean))
         (then-label-name (gensym "then-case"))
         (then-label (make-instance 'label :name then-label-name))
         (end-label-name (gensym "end"))
         (end-label (make-instance 'label :name end-label-name))
         (cond-branch (make-instance 'goto-if
                                     :target then-label-name
                                     :predicate pred-place))
         (end-branch (make-instance 'goto
                                    :target end-label-name)))
    (convert-expr (ir1:if-predicate expr)
                  :store-into pred-place
                  :program program
                  :procedure procedure)
    (push-instr procedure cond-branch)
    ;; compile the else-case first to avoid an extra branch
    (convert-expr (ir1:if-else-case expr)
                  :store-into store-into
                  :program program
                  :procedure procedure)
    (push-instr procedure end-branch)
    (push-instr procedure then-label)
    (convert-expr (ir1:if-then-case expr)
                  :store-into store-into
                  :program program
                  :procedure procedure)
    (push-instr procedure end-label)))

(defmethod convert-expr ((expr ir1:binop) &key store-into program procedure)
  (let* ((lhs-expr (ir1:binop-lhs expr))
         (rhs-expr (ir1:binop-rhs expr))
         (lhs (new-local procedure :type (repr-for-ir1-type (ir1:expr-type lhs-expr))))
         (rhs (new-local procedure :type (repr-for-ir1-type (ir1:expr-type rhs-expr))))
         (binop (make-instance 'binop
                               :dest store-into
                               :lhs lhs
                               :rhs rhs
                               :op (ir1:binop-op expr))))
    (convert-expr lhs-expr
                  :store-into lhs
                  :program program
                  :procedure procedure)
    (convert-expr rhs-expr
                  :store-into rhs
                  :program program
                  :procedure procedure)
    (push-instr procedure binop)))

(defun convert-and-discard (ir1-expr program procedure)
  (let ((place (new-local procedure :type (repr-for-ir1-type (ir1:expr-type ir1-expr)))))
    (convert-expr ir1-expr
                  :store-into place
                  :program program
                  :procedure procedure)))

(defmethod convert-expr ((expr ir1:prog2) &key store-into program procedure)
  (convert-and-discard (ir1:prog2-side-effect expr) program procedure)
  (convert-expr (ir1:prog2-return-value expr)
                :store-into store-into
                :program program
                :procedure procedure))

(defun collect-globals (ir1-prog program entry)
  "returns the first IR1:EXPR in IR1-PROG that is not a MONO-LET"
  (if (typep ir1-prog 'monomorphize:mono-let)
      (progn (convert-expr (mono-let-initform ir1-prog)
                         :store-into (add-global-place program ir1-prog)
                         :program program
                         :procedure entry)
             (collect-globals (mono-let-body ir1-prog) program entry))
      ir1-prog))

(defun transform-program (ir1-prog)
  (multiple-value-bind (program entry) (make-empty-program)
    (let* ((entry-after-globals (collect-globals ir1-prog program entry)))
      (convert-and-discard entry-after-globals program entry))
    program))
