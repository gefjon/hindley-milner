(uiop:define-package :hindley-milner/three-address/expr
  (:mix
   :hindley-milner/repr-type
   :hindley-milner/primop
   :hindley-milner/prologue
   :cl)
  (:shadow :type)
  (:export
   :register :type
   :local :name
   :constant :value
   
   :global :name :type

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

(define-enum register ((type repr-type))
  ((local ((name symbol)))
   (constant ((value t)))))

(define-class global ((name symbol)
                      (type repr-type)))

(deftype index ()
  '(and unsigned-byte fixnum))

(define-enum instr ()
  ((read-closure-env ((dst local)
                      (env register)
                      (index index)))
   (make-closure ((dst local)
                  (func global)
                  (elts (vector register))))
   (copy ((dst local)
          (src register)))
   (pointer-cast ((dst local)
                  (src register)))
   (primop ((op operator)
            (dst register)
            (args (vector register))))
   (branch ((condition register)
            (if-true symbol)
            (if-false symbol)))
   (call ((func local)
          (args (vector register))))

   ;; added in `liveness.lisp'
   (dead ((val local)))

   ;; added in `save-restore.lisp'
   (save ((src register)
          ;; `dst' should have type pointer-to (type src)
          (dst local)))
   (restore ((dst local)
             ;; `src' should have type pointer-to (type dst)
             (src local)))))

(define-class basic-block
    ((label (optional symbol))
     (body (vector instr))))

(define-class procedure
    ((name global)
     (args (vector local))
     (closure-env closure-env)
     (body (vector basic-block))))

(define-class program
    ((procs (vector procedure))
     (entry procedure)))
