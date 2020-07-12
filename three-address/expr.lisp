(uiop:define-package :hindley-milner/three-address/expr
  (:mix
   :hindley-milner/repr-type
   :hindley-milner/primop
   :hindley-milner/prologue
   :cl)
  (:shadow :type)
  (:export
   :register :name :type
   :local :global

   :index

   :instr
   :constant :dst :value
   :read-global :dst :src
   :set-global :dst :src
   :read-closure-env :dst :env :index
   :make-closure :dst :func :elts
   :copy :dst :src
   :primop :op :dst :args
   :pointer-cast :dst :src
   :branch :condition :if-true :if-false
   :call :func :args
   :dead :val
   :save :src :dst
   :restore :dst :src

   :basic-block :label :body
   
   :procedure :args :name :body :closure-env

   :global-def :name :initform

   :program :procs :entry :globals

   ;; reexports
   :type :operator)
  (:shadow :throw :ignore))
(cl:in-package :hindley-milner/three-address/expr)

(define-enum register ((name symbol)
                        (type type))
  ((local ())
   (global ())))

(deftype index ()
  '(and unsigned-byte fixnum))

(define-enum instr ()
  ((constant ((dst local)
              (value t)))
   (read-global ((dst local)
                 (src global)))
   (set-global ((dst global)
                (src local)))
   (read-closure-env ((dst local)
                      (env local)
                      (index index)))
   (make-closure ((dst local)
                  (func global)
                  (elts (vector local))))
   (copy ((dst local)
          (src local)))
   (pointer-cast ((dst local)
                  (src local)))
   (primop ((op operator)
            (dst local)
            (args (vector local))))
   (branch ((condition local)
            (if-true symbol)
            (if-false symbol)))
   (call ((func local)
          (args (vector local))))

   ;; added in `liveness.lisp'
   (dead ((val local)))

   ;; added in `save-restore.lisp'
   (save ((src local)
          ;; `dst' should have type pointer-to (type src)
          (dst local)))
   (restore ((dst local)
             ;; `src' should have type pointer-to (type dst)
             (src local)))
   ))

(define-class basic-block
    ((label (optional symbol))
     (body (vector instr))))

(define-class procedure
    ((name global)
     (args (vector local))
     (closure-env closure-env)
     (body (vector basic-block))))

(define-class global-def
    ((name global)
     (initform ; `nil' denotes 0-initialized and non-constant
               (optional t))))

(define-class program
    ((procs (vector procedure))
     (globals (vector global))
     (entry procedure)))
