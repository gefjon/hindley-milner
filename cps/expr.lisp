(uiop:define-package :hindley-milner/cps/expr
  (:mix
   :hindley-milner/cps/type
   :hindley-milner/prologue
   :cl)
  (:import-from :hindley-milner/primop
   :operator)
  (:import-from :hindley-milner/subst
   :subst-all-slots :subst-atom)
  (:shadow
   :type :variable :let :if :apply)
  (:export
   :variable :type
   :local :name
   :closure :name :corresponding-local
   :constant :value

   :closure-vars

   :*exit-continuation*

   :expr
   :alloc-struct :var :elts :in
   :read-struct :var :src :idx :in
   :transmute :new :old :in
   :let :var :prim-op :args :in
   :proc :name :body :closes-over :arglist :in
   :if :predicate :then-clause :else-clause
   :apply :func :args :continuation
   :throw :cont :arg))
(cl:in-package :hindley-milner/cps/expr)

(define-enum variable ((type repr-type))
  ((local ((name symbol)))
   (closure ((name symbol)
             (corresponding-local variable)))
   (constant ((value t))))
  :superclasses (subst-atom))

(deftype closure-vars ()
  '(vector closure))

(defvar *exit-continuation* (make-instance 'local
                                           :name 'exit
                                           :type (make-instance 'function
                                                                :inputs (specialized-vector repr-type *fixnum*)))
  "the continuation to exit the program")

(define-enum expr ()
  ((alloc-struct ((var local)
                  (elts (vector variable))
                  (in expr)))
   (read-struct ((var local)
                 (src variable)
                 (idx (and fixnum unsigned-byte))
                 (in expr)))
   (transmute ((new local)
               (old variable)
               (in expr)))
   (let ((var variable)
         (prim-op operator)
         (args (vector variable))
         (in expr)))
   (proc ((name local)
          (body expr)
          (closes-over closure-vars :may-init-unbound t)
          (arglist (vector local))
          (in expr)))
   (if ((predicate variable)
        (then-clause expr)
        (else-clause expr)))
   (apply ((func variable)
           (args (vector variable)))))
  :superclasses (subst-all-slots))
