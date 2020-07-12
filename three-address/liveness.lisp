(uiop:define-package :hindley-milner/three-address/liveness
  (:mix
   :hindley-milner/prologue
   :hindley-milner/three-address/expr
   :iterate
   :cl)
  (:export :liveness-annotate))
(in-package :hindley-milner/three-address/liveness)

(defgeneric liveness-annotate (thing))

(defmethod liveness-annotate ((program program))
  (map-slots #'liveness-annotate program))

(defmethod liveness-annotate ((vector vector))
  (map `(vector ,(array-element-type vector))
       #'liveness-annotate vector))

(defmethod liveness-annotate ((global-def global-def))
  global-def)

(defun push-onto-vector (src dst)
  (iter
    (for el in-vector src)
    (vector-push-extend el dst)))

(define-special *labels-to-live-set-map*
    (hash-map-of symbol (adjustable-vector local)))

(defun live-set-for-label (label)
  (ensure-get label *labels-to-live-set-map*
              (adjustable-vector local)))

(defmethod liveness-annotate
    ((procedure procedure)
     &aux (*labels-to-live-set-map* (make-hash-table :test #'eq)))
  (shallow-copy procedure
                :body (nreverse (map '(vector basic-block) #'liveness-annotate
                                     (reverse (body procedure))))))

(define-special *live-set* (adjustable-vector instr))
(define-special *current-bb-body* list)

(defmethod liveness-annotate
    ((bb basic-block)
     &aux (*live-set* (live-set-for-label (label bb)))
       *current-bb-body*)
  (map 'nil #'liveness-annotate (reverse (body bb)))
  (make-instance 'basic-block
                 :label (label bb)
                 :body (coerce *current-bb-body* '(vector instr))))

(defun add-all-to-live-set (vec)
  (push-onto-vector vec *live-set*))

(defmethod liveness-annotate :around ((branch branch))
  (add-all-to-live-set (live-set-for-label (if-true branch)))
  (add-all-to-live-set (live-set-for-label (if-false branch))))

(defmacro def-inputs (instr &rest slot-names)
  (flet ((recurse-on-slot (slot-name)
           `(inputs (slot-value ,instr ',slot-name))))
    `(defmethod inputs ((,instr ,instr))
       ,@(unless slot-names `((declare (ignorable ,instr))))
       (nconc ,@(mapcar #'recurse-on-slot slot-names)))))

(defgeneric inputs (instr)
  (:method ((local local)) (list local))
  (:method ((vector vector)) (coerce vector 'list)))
(def-inputs constant)
(def-inputs read-global)
(def-inputs set-global src)
(def-inputs read-closure-env env)
(def-inputs make-closure elts)
(def-inputs copy src)
(def-inputs pointer-cast src)
(def-inputs primop args)
(def-inputs branch condition)
(def-inputs call args)

(defun live-p (local)
  (find local *live-set* :test #'eq))

(defmethod liveness-annotate ((instr instr))
  (iter (for input in (inputs instr))
    (unless (live-p input)
      (push (make-instance 'dead :val input) *current-bb-body*))
    (vector-push-extend input *live-set*))
  (push instr *current-bb-body*)
  (values))
