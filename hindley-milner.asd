(uiop:define-package :hindley-milner.asd
  (:mix :asdf :cl)
  (:import-from :uiop
   :if-let :run-program))
(in-package :hindley-milner.asd)

(defparameter *header-names* '("gc" "types"))

(defclass runtime-c-source-file (c-source-file) ())

(defmethod output-files ((op compile-op) (c runtime-c-source-file))
  (list (merge-pathnames (make-pathname :type "o")
                         (component-pathname c))))

(defmethod input-files ((op compile-op) (c runtime-c-source-file)
                        &aux (pathname (component-pathname c)))
  (flet ((header-pathname (name)
           (merge-pathnames (make-pathname :name name
                                           :type "h")
                            pathname)))
    (cons pathname
          (mapcar #'header-pathname *header-names*))))

(defmethod perform ((op compile-op) (c runtime-c-source-file))
  (run-program (list "clang" "-Wall" "--std=c11" "-c"
                     "-o" (namestring (output-file op c))
                     (namestring (first (input-files op c))))
               :error-output t))

(defmethod perform ((op load-op) (c runtime-c-source-file))
  (declare (ignorable op c))
  (values))

(defsystem :hindley-milner/runtime
  :pathname "runtime"
  :components ((runtime-c-source-file "main")
               (runtime-c-source-file "gc")))

(defsystem :hindley-milner
  :class :package-inferred-system
  :author "Phoebe Goldman"
  :version "0.0.1"
  :depends-on (:hindley-milner/runtime :hindley-milner/compile))
