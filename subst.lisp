(uiop:define-package :hindley-milner/subst
  (:mix :hindley-milner/prologue :iterate :cl)
  (:import-from :gefjon-utils
   :map-slots)
  (:nicknames :subst)
  (:shadow :subst)
  (:export :subst :subst-all-slots :subst-atom :subst-recurse))
(cl:in-package :hindley-milner/subst)

(define-class subst-all-slots ()
  :documentation "A mixin which causes the generic `subst' to treat this `standard-class' as a trunk node, recursing on all its bound slots.")

(define-class subst-atom ()
  :documentation "A mixin which causes the generic `subst' to treat this `standard-class' as a leaf node, not recursing into it. Note that this is the default behavior, but this mixin exists for cases when it is necessary to override inherited behavior.")

(macrolet
    ((recursively (&body body)
       `(flet ((recurse (obj) (subst new old obj :test test)))
          ,@body)))
  (defgeneric subst-recurse (new old tree test)
    (:documentation "recurse on TREE during a call to `hindley-milner/subst:subst'")
    (:method (new old (leaf subst-atom) test)
      "The method for `subst-atom', which does not recurse. Return LEAF."
      (declare (ignorable new old test))
      leaf)
    (:method (new old leaf test)
      "The default method, which does not recurse. Return LEAF."
      (declare (ignorable new old test))
      leaf)
    (:method (new old (trunk subst-all-slots) test)
      "The method for `subst-all-slots', which recurses on all the bound slots of TRUNK.

Return a new instance of the same class as TRUNK, each of whose slots is the result of recursing on that slot-value of TRUNK."
      (recursively
       (map-slots #'recurse trunk)))
    (:method (new old (trunk cons) test)
      "Recurse on both the `car' and the `cdr' of TRUNK, constructing a new `cons'."
      (recursively
       (cons (recurse (car trunk))
             (recurse (cdr trunk)))))
    (:method (new old (trunk vector) test)
      "Recurse on all the elements of TRUNK, constructing a `vector'
with element-type (or (array-element-type TRUNK) (type-of NEW))."
      (recursively
       (map `(vector (or ,(array-element-type trunk)
                         ,(type-of new)))
            #'recurse trunk)))))

(defun subst (new old tree &key (test #'eql))
  "A generic version of `CL:SUBST', capable of recurring on trees other than `CONS'.

This function handles applying TEST to OLD and TREE; recursion on the
subfields of TREE is the responsibility of the generic function
`subst-recurse'. `standard-class'es should mix-in `subst-all-slots' to
gain a method; all other classes must define methods themselves."
  (if (funcall test old tree)
      new
      (subst-recurse new old tree test)))
