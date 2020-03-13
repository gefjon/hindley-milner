(uiop:define-package :hindley-milner/ir2/repr-type
    (:mix :hindley-milner/prologue :cl)
  (:import-from :hindley-milner/ir1
   :*boolean* :*fixnum* :*void*)
  (:nicknames :repr-type)
  (:export :repr-type :repr-for-ir1-type))
(cl:in-package :hindley-milner/ir2/repr-type)

(def-c-enum repr-type
  :boolean
  :fixnum
  :void
  :code-pointer)

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
