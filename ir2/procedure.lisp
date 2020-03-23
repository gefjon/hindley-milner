(uiop:define-package :hindley-milner/ir2/procedure
    (:mix
     :hindley-milner/prologue
     :hindley-milner/ir2/repr-type
     :hindley-milner/ir2/instr
     :hindley-milner/ir2/place
     :cl)
  (:shadowing-import-from :gefjon-utils
   :defclass :adjustable-vector :optional :make-adjustable-vector :|:| :->)
  (:shadowing-import-from :generic-cl
   :make-hash-map :get)
  (:export
   :procedure :procedure-name :procedure-arguments :procedure-locals :procedure-body
   :find-local :find-arg :find-local-function :add-local-function
   :make-empty-procedure
   :push-instr))
(cl:in-package :hindley-milner/ir2/procedure)

(defclass procedure
    ((name symbol)
     (function-type function-type)
     (arguments (vector argument))
     (locals (adjustable-vector local))
     (local-functions ; maps variable names to global procedure names
      (hash-map-of symbol symbol))
     (body (adjustable-vector instr))))

(|:| #'find-local (-> (symbol procedure) (optional local)))
(defun find-local (name procedure)
  (find name (procedure-locals procedure) :key #'place-name :from-end t))

(|:| #'find-arg (-> (symbol procedure) (optional argument)))
(defun find-arg (name procedure)
  (find name (procedure-arguments procedure) :key #'place-name :from-end t))

(|:| #'make-empty-procedure (-> (symbol sequence ir1-type:->) procedure))
(defun make-empty-procedure (name arguments ->-type)
  (make-instance 'procedure
                 :name name
                 :function-type (ftype-for-ir1-type ->-type)
                 :arguments (make-array (length arguments)
                                        :element-type 'argument
                                        :initial-contents arguments)
                 :locals (make-adjustable-vector :element-type local)
                 :local-functions (make-hash-map :test #'eq)
                 :body (make-adjustable-vector :element-type instr)))

(|:| #'find-local-function (-> (symbol procedure) (optional symbol)))
(defun find-local-function (var-name proc)
  (values (get var-name (procedure-local-functions proc))))

(|:| #'add-local-function (-> (symbol symbol procedure) (values &optional)))
(defun add-local-function (local-name global-name proc)
  (setf (get local-name (procedure-local-functions proc))
        global-name)
  (values))

(|:| #'push-instr (-> (procedure instr) (values &optional)))
(defun push-instr (procedure instr)
  (vector-push-extend instr (procedure-body procedure))
  (values))
