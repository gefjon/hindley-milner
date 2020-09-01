(uiop:define-package :hindley-milner/three-address/expr
  (:mix
   :hindley-milner/three-address/type
   :hindley-milner/primop
   :hindley-milner/prologue
   :cl)
  (:shadow :type :condition)
  (:export
   :register :type
   :local :name
   :constant :value
   
   :global :name :type

   :index

   :instr
   :read-global :dst :src
   :set-global :dst :src
   :read-struct :dst :src :index
   :make-struct :dst :elts :live-values
   :make-closure-func :dst :env :func
   :extract-env :dst :src
   :extract-func :dst :src
   :primop :op :dst :args
   :pointer-cast :dst :src
   :branch :condition :if-true :if-false
   :call :func :args
   :dead :val

   :basic-block :label :body
   
   :procedure :args :name :body :closure-env

   :global-def :name :initform

   :program :procs :entry :globals

   :*main*

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
  ((read-struct ((dst local)
                 (src register)
                 (index index)))
   (make-struct ((dst local)
                 (elts (vector local))
                 ;; added in `liveness.lisp'
                 (live-values (vector local) :may-init-unbound t)))
   (make-closure-func ((dst local)
                       (env local)
                       (func global)))
   (extract-env ((dst local)
                 (src local)))
   (extract-func ((dst local)
                  (src local)))
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
   (dead ((val local)))))

(define-class basic-block
    ((label (or null symbol))
     (body (vector instr))))

(define-class procedure
    ((name global)
     (args (vector local))
     (closure-env struct)
     (body (vector basic-block))))

(define-class program
    ((procs (vector procedure))
     (entry procedure)))

(defparameter *main* (make-instance 'global
                                    :name '|hm_main|
                                    :type (make-instance 'function-ptr :inputs (vector *opaque-ptr*
                                                                                 (make-instance 'function-ptr :inputs (vector *opaque-ptr* *fixnum*))))))
