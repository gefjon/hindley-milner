(uiop:define-package :hindley-milner/three-address/liveness
  (:mix
   :hindley-milner/repr-type
   :hindley-milner/prologue
   :hindley-milner/three-address/expr
   :iterate
   :cl)
  (:import-from :alexandria
   :curry :rcurry)
  (:export :liveness-annotate))
(in-package :hindley-milner/three-address/liveness)

(define-special *var-substitutions* hash-table)

(defun sub (orig new)
  (setf (gethash orig *var-substitutions*) new))

(defgeneric liveness-annotate (thing))

(defmethod liveness-annotate ((program program))
  (map-slots #'liveness-annotate program))

(defmethod liveness-annotate ((vector vector))
  (map `(vector ,(array-element-type vector))
       #'liveness-annotate vector))

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
     &aux (*var-substitutions* (make-hash-table :test #'eq))
       (*labels-to-live-set-map* (make-hash-table :test #'eq)))
  (shallow-copy procedure
                :body (nreverse (map '(vector basic-block) #'liveness-annotate
                                     (reverse (body procedure))))
                :args (apply-substitutions (args procedure))))

(define-special *live-set* (adjustable-vector instr))
(define-special *current-bb-body* list)

(defun make-live (local)
  (hash-set-insert local *live-set*))

(defgeneric add-instr (instr)
  (:method ((instr instr))
    (push instr *current-bb-body*)
    (values)))

(defstruct saved-tmp-info
  orig stack-slot new)

(defun die-if-not-live (local)
  (unless (live-p local)
    (push (make-instance 'dead :val local) *current-bb-body*))
  (values))

(defmethod add-instr
    ((instr make-closure)
     &aux (tmps (adjustable-vector saved-tmp-info)))
  (labels ((stack-slot-for (local)
             (make-instance 'local
                            :name (format-gensym "~a-stack-slot" (name local))
                            :type (make-instance 'stack-ptr
                                                 :pointee (type local))))
           (substitution-for (local)
             (shallow-copy local
                           :name (format-gensym "~a-copy" (name local))))
           (make-tmp-for-local (local)
             (when (contains-gc-ptr-p (type local))
               (vector-push-extend (make-saved-tmp-info
                                    :orig local
                                    :stack-slot (stack-slot-for local)
                                    :new (substitution-for local))
                                   tmps)))
           (save (tmp)
             (with-slot-accessors ((to-save saved-tmp-info-new)
                                   (stack-slot saved-tmp-info-stack-slot))
                 tmp
               (die-if-not-live to-save)
               (add-instr (make-instance 'save
                                         :dst stack-slot
                                         :src to-save))))
           (restore (tmp)
             (with-slot-accessors ((to-restore saved-tmp-info-orig)
                                   (stack-slot saved-tmp-info-stack-slot))
                 tmp
               (add-instr (make-instance 'dead :val stack-slot))
               (add-instr (make-instance 'restore
                                         :dst to-restore
                                         :src stack-slot))))
           (add-substitution (tmp)
             (sub (saved-tmp-info-orig tmp)
                  (saved-tmp-info-new tmp))))
    (hash-set-map #'make-tmp-for-local *live-set*)
    (map nil #'add-substitution tmps)
    (let* ((new-instr (shallow-copy instr
                                    :elts (apply-substitutions (elts instr)))))
      (mapc #'make-live (inputs new-instr))
      (map nil #'restore tmps)
      (call-next-method new-instr)
      (map nil #'save tmps))
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
                (delete-if (rcurry #'typep 'constant)
                           (nconc ,@(mapcar (curry #'recurse-on-slot method-name) slot-names))))))
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
(def-inputs-outputs read-closure-env (env) (dst))
(def-inputs-outputs make-closure (elts) (dst))
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

(defgeneric apply-substitutions (term)
  (:method (term) term)
  (:method ((term vector))
    (map `(vector ,(array-element-type term)) #'apply-substitutions term))
  (:method ((term instr))
    (map-slots #'apply-substitutions term))
  (:method ((term local))
    (multiple-value-bind (new-term repeat-p)
        (gethash term *var-substitutions* term)
      (if repeat-p (apply-substitutions new-term) new-term))))

(defmethod liveness-annotate
    ((orig instr)
     &aux (instr (apply-substitutions orig))
       (inputs (inputs instr)))
  (mapc #'make-unborn (outputs instr))
  (mapc #'die-if-not-live inputs)
  (add-instr instr)
  (mapc #'make-live inputs)
  (values))
