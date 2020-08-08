(uiop:define-package :hindley-milner.asd
  (:mix :asdf :cl)
  (:import-from :uiop
   :if-let :run-program)
  )
(in-package :hindley-milner.asd)

(defclass runtime-c-source-file (c-source-file) ())

(defmethod output-files ((op compile-op) (c runtime-c-source-file))
  (list (merge-pathnames (make-pathname :type "o")
                         (component-pathname c))))

(defun input-file (op c)
  (if-let ((input-files (input-files op c)))
    (progn (assert (= (length input-files) 1))
           (first input-files))
    (error "No input files for ~a on ~a" op c)))

(defmethod perform ((op compile-op) (c runtime-c-source-file))
  (run-program (list "clang" "-Wall" "--std=c11" "-c"
                     "-o" (namestring (output-file op c))
                     (namestring (input-file op c)))
               :error-output t))

(defmethod perform ((op load-op) (c runtime-c-source-file))
  (declare (ignorable op c))
  (values))

(defsystem :hindley-milner/runtime
  :pathname "runtime"
  :components ((runtime-c-source-file "main")))

(defsystem :hindley-milner
  :class :package-inferred-system
  :author "Phoebe Goldman"
  :version "0.0.1"
  :depends-on (:hindley-milner/runtime :hindley-milner/compile))
