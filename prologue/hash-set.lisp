(uiop:define-package :hindley-milner/prologue/hash-set
  (:mix :hindley-milner/prologue/typedec :cl)
  (:import-from :alexandria :rcurry)
  (:export
   :hash-set
   :make-hash-set
   :hash-set-contains-p
   :hash-set-insert
   :hash-set-remove
   :hash-set-map
   :hash-set-count
   :hash-set-vector))
(in-package :hindley-milner/prologue/hash-set)

(deftype hash-set (&optional eltype)
  (declare (ignore eltype))
  'hash-table)

(defun make-hash-set (&key (test #'eq))
  (make-hash-table :test test))

(|:| #'hash-set-contains-p (-> (t hash-set) (values t boolean)))
(defun hash-set-contains-p (elt set)
  "Test is SET contains ELT.

If present, return the version in the set as a primary value and `t' as a secondary value.
If not present, return `nil' as both primary and secondary values."
  (multiple-value-bind (entry foundp) (gethash elt set)
    (values
     (when foundp
       (assert (funcall (hash-table-test set) elt entry))
       entry)
     foundp)))

(|:| #'hash-set-insert (-> (t hash-set) void))
(defun hash-set-insert (elt set)
  "Insert ELT into SET.

Does no checking to see if SET already contains ELT."
  (setf (gethash elt set) elt)
  (values))

(|:| #'hash-set-remove (-> (t hash-set) boolean))
(defun hash-set-remove (elt set)
  "Remove ELT from SET, returning `t' if it was present or `nil' if it was not."
  (remhash elt set))

(|:| #'hash-set-map (-> ((-> (t) void) hash-set) void))
(defun hash-set-map (func set)
  (flet ((maphash-func (key val)
           (assert (funcall (hash-table-test set) key val))
           (funcall func key)))
    (maphash #'maphash-func set))
  (values))

(|:| #'hash-set-count (-> (hash-set) unsigned-byte))
(defun hash-set-count (set)
  (hash-table-count set))

(|:| #'hash-set-vector (-> (hash-set) vector))
(defun hash-set-vector (set
                        &aux (vec (make-array (hash-set-count set)
                                              :adjustable t
                                              :fill-pointer 0)))
  (hash-set-map (rcurry #'vector-push vec) set)
  vec)
