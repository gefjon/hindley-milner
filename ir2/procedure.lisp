(uiop:define-package :hindley-milner/ir2/procedure
    (:mix
     :hindley-milner/ir2/instr
     :hindley-milner/ir2/place
     :cl)
  (:shadowing-import-from :gefjon-utils
   :defclass :adjustable-vector :optional :make-adjustable-vector :|:| :->)
  (:export
   :procedure :procedure-name :procedure-arguments :procedure-locals :procedure-body
   :find-local :find-arg
   :make-empty-procedure
   :push-instr))
(cl:in-package :hindley-milner/ir2/procedure)

(defclass procedure
    ((name symbol)
     (arguments (adjustable-vector argument))
     (locals (adjustable-vector local))
     (body (adjustable-vector instr))))

(|:| #'find-local (-> (symbol procedure) (optional local)))
(defun find-local (name procedure)
  (find name (procedure-locals procedure) :key #'place-name))

(|:| #'find-arg (-> (symbol procedure) (optional argument)))
(defun find-arg (name procedure)
  (find name (procedure-arguments procedure) :key #'place-name))

(defun make-empty-procedure (name arguments)
  (make-instance 'procedure
                 :name name
                 :arguments (make-adjustable-vector :element-type argument
                                                    :initial-contents arguments)
                 :locals (make-adjustable-vector :element-type local)
                 :body (make-adjustable-vector :element-type instr)))

(|:| #'push-instr (-> (procedure instr) (values &optional)))
(defun push-instr (procedure instr)
  (vector-push-extend instr (procedure-body procedure))
  (values))
