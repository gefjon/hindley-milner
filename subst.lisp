(uiop:define-package :hindley-milner/subst
  (:mix :hindley-milner/prologue :iterate :cl)
  (:import-from :gefjon-utils
   :map-slots)
  (:nicknames :subst)
  (:shadow :subst)
  (:export :subst))
(cl:in-package :hindley-milner/subst)

(define-class subst-all-slots ()
  :documentation "A mixin which causes the generic `subst' to treat this `standard-class' as a trunk node, recursing on all its bound slots.")

(macrolet
    ((recursively (&body body)
       `(flet ((recurse (obj) (subst new old obj :test test)))
          ,@body)))
  (defgeneric subst-recurse (new old tree test)
    (:documentation "recurse on TREE during a call to `hindley-milner/subst:subst'")
    (:method (new old leaf test)
      "The default method, which does not recurse."
      (declare (ignorable new old test))
      leaf)
    (:method (new old (trunk subst-all-slots) test)
      (recursively
       (map-slots #'recurse trunk)))
    (:method (new old (trunk cons) test)
      (recursively
       (cons (recurse (car trunk))
             (recurse (cdr trunk)))))
    (:method (new old (trunk vector) test)
      (recursively
       (map (type-of trunk) #'recurse trunk)))))

(defun subst (new old tree &key (test #'eql))
  "a generic version of `CL:SUBST', capable of recurring on trees other than `CONS'

define recursive methods using `HINDLEY-MILNER/SUBST:DEFINE-SUBST'"
  (if (funcall test old tree)
      new
      (subst-recurse new old tree test)))
