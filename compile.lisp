(uiop:define-package :hindley-milner/compile
  (:nicknames :compile)
  (:mix :hindley-milner/prologue :cl)
  (:import-from :uiop
   :run-program)
  (:import-from :asdf
   :output-file :make-operation :compile-op :find-component :find-system)
  (:shadow :compile)
  (:import-from :hindley-milner/syntax
   :read-program-from-file)
  (:import-from :hindley-milner/ir1
   :ir1-trans)
  (:import-from :hindley-milner/cps
   :cps-trans)
  (:import-from :hindley-milner/three-address
   :3adr-transform)
  (:import-from :hindley-milner/ir4
   :ir4-trans)
  (:import-from :hindley-milner/llvm-emit
   :emit-to-file)
  (:export :compile :*compile-output* :*compile-error*))
(in-package :hindley-milner/compile)

(|:| #'compile-to-ll (-> (pathname pathname) void))
(defun compile-to-ll (infile ll-outfile)
  (let* ((surface-syntax (read-program-from-file infile))
         (ir1 (ir1-trans surface-syntax))
         (cps (cps-trans ir1))
         (three-address (3adr-transform cps))
         (ir4 (ir4-trans three-address)))
    (emit-to-file ll-outfile ir4))
  (values))

(|:| *compile-output* stream)
(defvar *compile-output* *standard-output*)
(|:| *compile-error* stream)
(defvar *compile-error* *error-output*)

(defparameter *runtime-components* '("main" "gc"))

(defun runtime-o-path (component)
  (namestring
   (output-file (make-operation 'compile-op)
                (find-component (find-system :hindley-milner/runtime)
                                component))))

(defparameter *runtime-o-paths* (mapcar #'runtime-o-path *runtime-components*))

(|:| #'link (-> (pathname pathname) void))
(defun link (ll-infile exe-outfile)
  (run-program (append (list "clang" "-Wall" "-std=c11"
                             "-o" (namestring exe-outfile)
                             (namestring ll-infile))
                       *runtime-o-paths*)
               :error-output *compile-error*
               :output *compile-output*))

(|:| #'asmify (-> (pathname pathname) void))
(defun asmify (ll-infile s-outfile)
  (run-program (list "llc"
                     "-o" (namestring s-outfile)
                     (namestring ll-infile))
               :error-output *compile-error*
               :output *compile-output*))

(|:| #'compile (-> ((or string pathname) (or string pathname)) void))
(defun compile (infile outfile)
  (let* ((source-path (pathname infile))
         (exe-path (pathname outfile))
         (s-path (merge-pathnames (make-pathname :type "s")
                                  exe-path))
         (ll-path (merge-pathnames (make-pathname :type "ll")
                                   exe-path)))
    (compile-to-ll source-path ll-path)
    (asmify ll-path s-path)
    (link s-path exe-path))
  (values))
