(uiop:define-package :hindley-milner/syntax
  (:nicknames :syntax)
  (:mix
   :hindley-milner/syntax/clause
   :iterate
   :hindley-milner/prologue
   :named-readtables
   :cl)
  (:import-from :alexandria :with-gensyms)
  (:import-from :hindley-milner/syntax/package)
  (:import-from :hindley-milner/primop
   :operator)
  (:import-from :hindley-milner/syntax/parse
   :parse :parse-def)
  (:mix-reexport
   :hindley-milner/syntax/clause)
  (:export
   :hindley-milner ;; names the readtable

   :program :definitions :entry
   :read-program-from-file))
(cl:in-package :hindley-milner/syntax)

(define-class program
  ((definitions (vector definition))
   (entry clause)))

(defreadtable hindley-milner
  (:merge :standard)
  (:case :preserve))

(defun read-program-from-file (file)
  (cl:let ((*readtable* (find-readtable 'hindley-milner))
           (*package* (find-package :hm)))
    (iter
      (with definitions = (make-adjustable-vector :element-type definition))
      (with entry)
      (for form in-file file)
      (when entry
        (vector-push-extend (parse-def entry) definitions))
      (setf entry form)
      (finally (return
                 (make-instance 'program
                                :definitions definitions
                                :entry (parse entry)))))))
