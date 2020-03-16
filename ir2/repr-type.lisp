(uiop:define-package :hindley-milner/ir2/repr-type
    (:mix :iterate :hindley-milner/prologue :cl)
  (:import-from :hindley-milner/ir1
   :*boolean* :*fixnum* :*void*)
  (:shadowing-import-from :gefjon-utils
   :defclass)
  (:nicknames :repr-type)
  (:export
   :repr-type
   :function-type :function-type-inputs :function-type-result
   :repr-for-ir1-type :ftype-for-ir1-type))
(cl:in-package :hindley-milner/ir2/repr-type)

(def-c-enum repr-type
  :boolean
  :fixnum
  :void
  :code-pointer)

(defclass function-type
    ((inputs (vector repr-type))
     (result repr-type)))

(defmacro evcase (keyform &rest clauses)
  "like ECASE, but evaluates the keyforms of each of the CLAUSES

this permits forms like (EVCOND TYPE (*BOOLEAN* :BOOLEAN))"
  (alexandria:once-only (keyform)
    (flet ((transform-clause (clause)
             (destructuring-bind (valform &body body) clause
               `((eq ,valform ,keyform) ,@body))))
      `(cond
         ,@(mapcar #'transform-clause clauses)
         (t (error "fell through EVCASE"))))))

(defgeneric repr-for-ir1-type (ir1-type))

(defmethod repr-for-ir1-type ((ir1-type ir1-type:type-primitive))
  (evcase ir1-type
    (*boolean* :boolean)
    (*fixnum* :fixnum)
    (*void* :void)))

(defmethod repr-for-ir1-type ((ir1-type ir1-type:->))
  (declare (ignorable ir1-type))
  :code-pointer)

(defun ftype-for-ir1-type (ir1-type:->)
  (iter
    (for -> first ir1-type:-> then (ir1-type:->-output ->))
    (while (typep -> 'ir1-type:->))
    (for in-type = (ir1-type:->-input ->))
    (for in-repr = (repr-for-ir1-type in-type))
    (collect in-repr into inputs at end result-type '(vector repr-type))
    (finally
     (return (make-instance 'function-type
                            :inputs inputs
                            :result (repr-for-ir1-type ->))))))
