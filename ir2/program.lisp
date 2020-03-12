(uiop:define-package :hindley-milner/ir2/program
    (:mix
     :hindley-milner/prologue
     :hindley-milner/ir2/procedure
     :cl)
  (:shadowing-import-from :gefjon-utils
   :defclass :adjustable-vector :make-adjustable-vector :|:| :->)
  (:shadowing-import-from :generic-cl
   :make-hash-map :get)
  (:export
   :program :program-procedures :program-globals :program-entry
   :find-global
   :add-procedure
   :make-empty-program))
(cl:in-package :hindley-milner/ir2/program)

(defclass program
    ((procedures (hash-map-of symbol procedure))
     (entry-point symbol)
     (globals (adjustable-vector place))))

(|:| #'find-global (-> (symbol program) (optional place)))
(defun find-global (name program)
  (find name (program-globals program) :key #'place-name))

(|:| #'add-procedure (-> (program procedure) (values &optional)))
(defun add-procedure (program procedure)
  (setf (get (procedure-name procedure) (program-procedures program))
        procedure)
  (values))

(defun make-empty-program ()
  "returns (VALUES PROGRAM ENTRY-PROCEDURE)"
  (let* ((entry-point (gensym "main"))
         (entry-procedure (make-empty-procedure entry-point ()))
         (program (make-instance 'program
                                 :procedures (make-hash-map :test #'eq)
                                 :entry-point entry-point
                                 :globals (make-adjustable-vector :element-type place))))
    (add-procedure program entry-procedure)
    (values program entry-procedure)))

