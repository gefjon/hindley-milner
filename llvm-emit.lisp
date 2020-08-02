(uiop:define-package :hindley-milner/llvm-emit
  (:mix
   :hindley-milner/prologue
   :hindley-milner/ir4
   :cl
   :iterate)
  (:import-from :alexandria
   :string-designator)
  (:export :emit-to-file))
(in-package :hindley-milner/llvm-emit)

(define-special *emit-out* stream)

(|:| #'write-out (-> (string-designator) void))
(defun write-out (str)
  (write-string (string str) *emit-out*)
  (values))

(defun fmt (control &rest args)
  (apply #'format *emit-out* control args)
  (values))

(defun newline ()
  (write-out #\newline))

(defun space ()
  (write-out #\space))

(defun comma ()
  (write-out ", "))

(defun indent ()
  (write-out "  "))

(defgeneric emit (thing))

(|:| #'emit-to-file (-> ((or string pathname) program) void))
(defun emit-to-file (filename program)
  (with-open-file (*emit-out* filename :direction :output
                                       :if-exists :supersede)
    (emit program)))

(defmethod emit ((program program))
  (map nil #'emit (procs program))
  (emit (entry program)))

(defmethod emit ((local local))
  (fmt "%~a" (name local)))

(defmethod emit ((global global))
  (fmt "@~a" (name global)))

(|:| #'emit-arglist (-> ((vector (cons repr-type val))) void))
(defun emit-arglist (vec)
  (write-out #\()
  (iter
    (for (type . val) in-vector vec)
    (unless (first-time-p)
      (comma))
    (emit type)
    (space)
    (emit val))
  (write-out #\)))

(defmethod emit ((proc procedure))
  (write-out "define \"tailcc\" void ")
  (emit (name proc))
  (space)
  (emit-arglist (args proc))
  (write-out " gc \"statepoint-example\" {")
  (newline)
  (map nil #'emit (body proc))
  (write-out #\})
  (newline))

(defmethod emit ((bb basic-block))
  (when (label bb)
    (write-out (label bb))
    (write-out #\:)
    (newline))
  (map nil #'emit (body bb)))

(defmethod emit ((instr br))
  (write-out "br i1 ")
  (emit (cond instr))
  (comma)
  (write-out "label ")
  (write-out (if-true instr))
  (comma)
  (write-out "label ")
  (write-out (if-false instr))
  (newline))

(defmethod emit ((instr tailcall))
  (write-out "musttail call void ")
  (emit (func instr))
  (emit-arglist (args instr))
  (newline))

(defvar *zero* (make-instance 'const :val 0))

(defvar *alloc-fn* (make-instance 'pointer
                                  :pointee
                                  (make-instance 'function
                                                 :inputs (specialized-vector repr-type *i64*)
                                                 :output (make-instance 'pointer :pointee *i8*))))
(defvar *gc-alloc* (make-instance 'global
                                  :name '|gcalloc|))

(defmethod emit ((instr alloc-call))
  (emit (dst instr))
  (write-out " = notail call token @llvm.experimental.gc.statepoint")
  (emit-arglist (concatenate '(vector (cons repr-type val))
                             (list (cons *i64* *zero*) ; id
                                   (cons *i32* *zero*) ; patch bytes
                                   (cons *alloc-fn* *gc-alloc*) ; callee
                                   (cons *i64* *zero*) ; flags
                                   (arg instr) ; args
                                   (cons *i64* *zero*) ; trans arg count
                                   (cons *i64* *zero*)) ; deopt arg count
                             (live-ptrs instr)))
  (newline))

(defmethod emit ((instr alloc-relocate))
  (emit (dst instr))
  (write-out " = notail call ")
  (emit (type instr))
  (write-out " @llvm.experimental.gc.relocate (token ")
  (emit (token instr))
  (comma)
  (emit *i32*)
  (space)
  (emit (index instr))
  (comma)
  (emit *i32*)
  (space)
  (emit (index instr))
  (write-out #\))
  (newline))

(defmethod emit ((instr alloc-result))
  (emit (dst instr))
  (write-out " = notail call ")
  (emit (type instr))
  (write-out " @llvm.experimental.gc.result (token ")
  (emit (token instr))
  (write-out #\))
  (newline))

(defmethod emit ((instr bitcast))
  (emit (dst instr))
  (write-out " = bitcast ")
  (emit (in-ty instr))
  (space)
  (emit (in instr))
  (write-out " to ")
  (emit (out-ty instr))
  (newline))

(defmethod emit ((instr ptrtoint))
  (emit (dst instr))
  (write-out " = ptrtoint ")
  (emit (in-ty instr))
  (space)
  (emit (in instr))
  (write-out " to ")
  (emit (out-ty instr))
  (newline))

(defmethod emit ((instr extractvalue))
  (emit (dst instr))
  (write-out " = extractvalue ")
  (emit (agg-ty instr))
  (space)
  (emit (agg instr))
  (iter
    (for index in-vector (indices instr))
    (comma)
    (emit index))
  (newline))

(defmethod emit ((instr insertvalue))
  (emit (dst instr))
  (write-out " = insertvalue ")
  (emit (agg-ty instr))
  (space)
  (emit (agg instr))
  (comma)
  (emit (field-ty instr))
  (space)
  (emit (field instr))
  (iter
    (for index in-vector (indices instr))
    (comma)
    (emit index))
  (newline))

(defmethod emit ((instr getelementptr))
  (emit (dst instr))
  (write-out " = getelementptr ")
  (emit (agg-ty instr))
  (comma)
  (emit (ptr-ty instr))
  (space)
  (emit (ptr instr))
  (iter
    (for (type . index) in-vector (indices instr))
    (comma)
    (emit type)
    (space)
    (emit index))
  (newline))

(defmethod emit ((instr load))
  (emit (dst instr))
  (write-out " = load ")
  (emit (ty instr))
  (comma)
  (emit (ptr-ty instr))
  (space)
  (emit (ptr instr))
  (newline))

(defmethod emit ((instr store))
  (write-out "store ")
  (emit (ty instr))
  (space)
  (emit (val instr))
  (comma)
  (emit (ptr-ty instr))
  (space)
  (emit (ptr instr))
  (newline))

(defmethod emit ((instr arith))
  (emit (dst instr))
  (write-out " = ")
  (write-out (string-downcase (symbol-name (op instr))))
  (space)
  (emit (ty instr))
  (space)
  (emit (lhs instr))
  (comma)
  (emit (rhs instr))
  (newline))

(defmethod emit ((instr ret))
  (write-out "ret void")
  (newline))

(defmethod emit ((type token))
  (write-out "token"))

(defmethod emit ((type void))
  (write-out "void"))

(defmethod emit ((int fixnum))
  (princ int *emit-out*))

(defmethod emit ((type integer))
  (write-out #\i)
  (emit (bitwidth type)))

(defmethod emit ((type function))
  (emit (output type))
  (space)
  (write-out #\()
  (iter (for input in-vector (inputs type))
    (unless (first-time-p) (comma))
    (emit input))
  (write-out #\)))

(defmethod emit ((type pointer))
  (emit (pointee type))
  (write-out #\*))

(defmethod emit ((type struct))
  (write-out #\{)
  (iter (for elt in-vector (members type))
    (if-first-time (space) (comma))
    (emit elt))
  (write-out " }"))

(defmethod emit ((val const))
  (emit (val val)))

(defmethod emit ((val undef))
  (declare (ignorable val))
  (write-out "undef"))

(defmethod emit ((val nullptr))
  (declare (ignorable val))
  (write-out "null"))

(defmethod emit :before ((instr instr))
  (declare (ignorable instr))
  (indent))
