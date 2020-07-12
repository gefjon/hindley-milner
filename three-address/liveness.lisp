(uiop:define-package :hindley-milner/three-address/liveness
  (:mix
   :hindley-milner/repr-type
   :hindley-milner/prologue
   :hindley-milner/three-address/expr
   :iterate
   :cl)
  (:import-from :alexandria
   :curry)
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
    (hash-map-of symbol (hash-set local)))

(defun live-set-for-label (label)
  (ensure-get label *labels-to-live-set-map*
              (make-hash-set :test #'eq)))

(defmethod liveness-annotate
    ((procedure procedure)
     &aux (*labels-to-live-set-map* (make-hash-table :test #'eq)))
  (shallow-copy procedure
                :body (nreverse (map '(vector basic-block) #'liveness-annotate
                                     (reverse (body procedure))))))

(define-special *live-set* (adjustable-vector instr))
(define-special *current-bb-body* list)

(defun make-live (local)
  (hash-set-insert local *live-set*))

(defgeneric add-instr (instr)
  (:method ((instr instr))
    (push instr *current-bb-body*)
    (values)))

(defmethod add-instr :around
    ((instr make-closure)
     &aux (tmps (adjustable-vector (cons local local))))
  (labels ((stack-slot-for (local)
             (make-instance 'local
                            :name (format-gensym "~a-stack-slot" (name local))
                            :type (make-instance 'pointer
                                                 :pointee (type local))))
           (make-tmp-for-local (local)
             (vector-push-extend (cons local (stack-slot-for local))
                                 tmps))
           (save (tmp)
             (destructuring-bind (to-save . stack-slot) tmp
               (add-instr (make-instance 'save
                                         :dst stack-slot
                                         :src to-save))))
           (restore (tmp)
             (destructuring-bind (to-rest . stack-slot) tmp
               (add-instr (make-instance 'restore
                                         :dst to-rest
                                         :src stack-slot)))))
    (hash-set-map #'make-tmp-for-local *live-set*)
    (map nil #'restore tmps)
    (call-next-method)
    (map nil #'save tmps)
    (values)))

(defmethod liveness-annotate
    ((bb basic-block)
     &aux (*live-set* (live-set-for-label (label bb)))
       *current-bb-body*)
  (map 'nil #'liveness-annotate (reverse (body bb)))
  (make-instance 'basic-block
                 :label (label bb)
                 :body (coerce *current-bb-body* '(vector instr))))

(|:| #'merge-into-live-set (-> ((hash-set local)) void))
(defun merge-into-live-set (other-set)
  (hash-set-map #'make-live other-set)
  (values))

(defmethod liveness-annotate :before ((branch branch))
  (merge-into-live-set (live-set-for-label (if-true branch)))
  (merge-into-live-set (live-set-for-label (if-false branch))))

(defmacro def-inputs-outputs (instr &optional inputs outputs)
  (labels ((recurse-on-slot (func slot-name)
             `(,func (slot-value ,instr ',slot-name)))
           (defmethod-form (method-name slot-names)
             `(defmethod ,method-name ((,instr ,instr))
                ,@(unless slot-names `((declare (ignorable ,instr))))
                (nconc ,@(mapcar (curry #'recurse-on-slot method-name) slot-names)))))
    `(progn
       ,(defmethod-form 'inputs inputs)
       ,(defmethod-form 'outputs outputs))))

(defgeneric inputs (instr)
  (:method ((local local)) (list local))
  (:method ((vector vector)) (coerce vector 'list)))
(defgeneric outputs (instr)
  (:method ((local local)) (list local))
  (:method ((vector vector)) (coerce vector 'list)))

(def-inputs-outputs constant)
(def-inputs-outputs read-global)
(def-inputs-outputs set-global (src))
(def-inputs-outputs read-closure-env (env) (dst))
(def-inputs-outputs make-closure (elts) (dst))
(def-inputs-outputs copy (src) (dst))
(def-inputs-outputs pointer-cast (src) (dst))
(def-inputs-outputs primop (args) (dst))
(def-inputs-outputs branch (condition))
(def-inputs-outputs call (args))

(defun live-p (local)
  (multiple-value-bind (_ res)
      (hash-set-contains-p local *live-set*)
    (declare (ignore _))
    res))

(defun make-unborn (local)
  (hash-set-remove local *live-set*)
  (values))

(defun use (local)
  (unless (live-p local)
    (push (make-instance 'dead :val local) *current-bb-body*))
  (make-live local)
  (values))

(defmethod liveness-annotate ((instr instr))
  (mapc #'use (inputs instr))
  (add-instr instr)
  (mapc #'make-unborn (outputs instr))
  (values))
