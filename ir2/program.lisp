(uiop:define-package :hindley-milner/ir2/program
    (:mix
     :hindley-milner/prologue
     :hindley-milner/ir2/place
     :hindley-milner/ir2/procedure
     :hindley-milner/ir2/repr-type
     :cl)
  (:shadowing-import-from :gefjon-utils
   :defclass :adjustable-vector :optional :make-adjustable-vector :|:| :->)
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
     (globals (adjustable-vector global))))

(|:| #'find-global (-> (symbol program) (optional global)))
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
         (entry-ftype (make-instance 'ir1-type:->
                                     :input ir1-type:*void*
                                     :output ir1-type:*void*))
         (entry-procedure (make-empty-procedure entry-point () entry-ftype))
         (program (make-instance 'program
                                 :procedures (make-hash-map :test #'eq)
                                 :entry-point entry-point
                                 :globals (make-adjustable-vector :element-type global))))
    (add-procedure program entry-procedure)
    (values program entry-procedure)))

