(uiop:define-package :hindley-milner/three-address/expr
  (:mix
   :hindley-milner/repr-type
   :hindley-milner/primop
   :hindley-milner/prologue
   :cl)
  (:export
   :register :register-name :register-type

   :index :closure-env

   :instr
   :dead :reg
   :read-global :dst :global
   :set-global :global :src
   :read-closure-env :dst :index
   :make-closure-env :dst :procedure :elts
   :load-constant :dst :index
   :copy :dst :src
   :primop :op :dst :args
   :param :src
   :call :condition :arity :closure-env :continuation :func
   
   :procedure :args :name :body :continuation-arg

   :program :procs :entry :globals :constants

   ;; reexports
   :type :operator)
  (:shadow :throw :ignore))
(cl:in-package :hindley-milner/three-address/expr)

(define-enum register ((name symbol)
                       (type type))
  ((reg ())))

(deftype closure-env ()
  '(vector type))

(deftype index ()
  '(and unsigned-byte fixnum))

(define-enum instr ()
  ((dead ((reg register)))
   (read-global ((dst register)
                 (global index)))
   (set-global ((global index)
                (src register)))
   (read-closure-env ((dst register)
                      (index index)))
   (make-closure-env ((dst register)
                      (elts (vector register))))
   (load-constant ((dst register)
                   (index index)))
   (copy ((dst register)
          (src register)))
   (primop ((op operator)
            (dst register)
            (args (vector register))))
   (param ((src register)))
   (call ((condition ; `t' denotes always taken, `nil' denotes never taken
           (or register boolean))
          (arity index)
          (closure-env ; `nil' denotes the current env
           (or register null))
          (continuation ; `nil' denotes the current continuation
           (or register index null))
          (func (or register index))))))

(define-class procedure
    ((name symbol)
     (args (vector register))
     (continuation-arg (or register null))
     (closure-env closure-env)
     (body (adjustable-vector instr))))

(define-class program
    ((procs (adjustable-vector procedure))
     (globals (adjustable-vector type))
     (entry procedure)
     (constants (adjustable-vector t))))
