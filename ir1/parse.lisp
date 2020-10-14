(uiop:define-package :hindley-milner/ir1/parse
  (:mix
   :hindley-milner/ir1/expr
   :hindley-milner/prologue
   :cl
   :iterate)
  (:import-from :trivial-types :proper-list)
  (:import-from :hindley-milner/syntax)
  (:import-from :uiop :emptyp)
  (:export :parse-program))
(cl:in-package :hindley-milner/ir1/parse)

(define-special *types* (vector type))

(|:| #'add-type (-> (type-scheme) void))
(defun add-type (new-type)
  (vector-push-extend new-type *types*)
  (values))

(defgeneric parse (clause)
  (:documentation "transform a `SYNTAX:CLAUSE' into an `IR1:EXPR'"))

(defgeneric parse-definition (def))

(defmethod parse-definition ((def syntax:const))
  (make-instance 'definition
                 :name (syntax:binding def)
                 :initform (parse (syntax:value def))))

(define-special *type-variables* (hash-map-of symbol type-variable))

(|:| #'add-type-var (-> (symbol) type-variable))
(defun add-type-var (name)
  (setf (gethash name *type-variables*)
        (new-type-variable name)))

(|:| #'find-type-var (-> (symbol) type-variable))
(defun find-type-var (name)
  (multiple-value-bind (var foundp) (gethash name *type-variables*)
    (unless foundp
      (error "Unbound type variable ~a" name))
    var))

(|:| #'parse-type-name (-> (symbol) type))
(defun parse-type-name (type-name)
  (case type-name
    (hm:|fixnum| *fixnum*)
    (hm:|boolean| *boolean*)
    (hm:|void| *void*)
    (otherwise (find-type-var type-name))))

(|:| #'parse-struct-def (-> (syntax:struct) struct))
(defun parse-struct-def (struct)
  (let* ((elements (map '(vector type) #'parse-type-name
                        (syntax:elts struct))))
    (make-instance 'struct
                   :name (syntax:name struct)
                   :elts elements)))

(defmethod parse-definition ((def syntax:struct))
  (let* ((*type-variables* (make-hash-table :test #'eq))
         (bindings (map 'list #'add-type-var
                        (syntax:type-params def))))
    (add-type (make-instance 'type-scheme
                             :bindings bindings
                             :body (parse-struct-def def)))))

(defmethod parse-definition ((def syntax:enum))
  (let* ((*type-variables* (make-hash-table :test #'eq))
         (bindings (map 'list #'add-type-var
                        (syntax:type-params def)))
         (variants (map '(vector struct) #'parse-struct-def
                        (syntax:variants def)))
         (enum (make-instance 'enum
                              :name (syntax:name def)
                              :variants variants)))
    (add-type (make-instance 'type-scheme
                             :bindings bindings
                             :body enum))))

(defmethod parse-definition ((def syntax:fn))
  (make-instance 'definition
                 :name (syntax:name def)
                 :initform (parse (syntax:value def))))

(|:| #'transform-implicit-progn (-> ((vector syntax:clause)) expr))
(defun transform-implicit-progn (progn)
  (cond ((emptyp progn) (error "empty implicit progn"))
        ;; if there's only one clause, you can skip consing the progn
        ((= (length progn) 1) (parse (aref progn 0)))
        (:otherwise
         (flet ((reduce-prog2 (side-effect return-value)
                  (make-instance 'prog2
                                 :side-effect side-effect
                                 :return-value return-value)))
           (reduce #'reduce-prog2 progn :key #'parse)))))

(defmethod parse ((arg-vec vector))
  (iter
    (for arg in-vector arg-vec)
    (collect (parse arg) result-type (vector expr))))

(defgeneric parse-pattern (pat discrim-var)
  (:documentation "Returns two values, a PREDICATE expr and a list `definition's for bindings in the chosen arm's expr."))

(defmethod parse-pattern ((bind syntax:bind) discrim-var)
  (values *true*
          (list (make-instance 'definition
                               :name (syntax:name bind)
                               :initform discrim-var))))

(defmethod parse-pattern ((exactly syntax:exactly) discrim-var)
  (let* ((constant (make-instance 'quote :it (syntax:value exactly)))
         (primop (make-instance 'primop
                           :op 'hm:=
                           :args (specialized-vector expr
                                                     constant
                                                     discrim-var))))
    (values primop nil)))

(defmethod parse-pattern ((ign syntax:ign) discrim-var)
  (declare (ignorable ign discrim-var))
  (values *true* nil))

(defun andify (lhs rhs)
  (make-instance 'and :lhs lhs :rhs rhs))

(defun condition-union (&rest conditions)
  (cond
    ((null conditions) *true*)
    ((cl:and (consp conditions)
             (null (rest conditions)))
     (first conditions))
    ((reduce #'andify conditions))))

(defmethod parse-pattern ((destruct syntax:destruct) discrim-var)
  (iter
    (with ctor = (syntax:name destruct))
    (with discrim-p = (make-instance 'discriminant-p
                                     :variant ctor
                                     :value discrim-var))
    (with transmute-sym = (make-gensym 'transmute))
    (with transmute-var = (make-instance 'variable :name transmute-sym))
    (with transmute-expr = (make-instance 'assert-variant
                                          :src discrim-var
                                          :constructor ctor))
    (with transmute-def = (make-instance 'definition
                                         :name transmute-sym
                                         :initform transmute-expr))
    (for subpat in-vector (syntax:elts destruct))
    (for field-idx upfrom 0)
    (for sym = (format-gensym "field-~a-" field-idx))
    (for var = (make-instance 'variable :name sym))
    (for def = (make-instance 'definition
                              :name sym
                              :initform (make-instance 'field-value
                                                       :idx field-idx
                                                       :constructor ctor
                                                       :aggregate transmute-var)))
    (for (values subpred subbinds) = (parse-pattern subpat var))
    (collect (make-instance 'let
                            :def def
                            :body subpred)
      into predicates)
    (collect def into binds)
    (appending subbinds into binds)
    (finally (return (values (andify discrim-p
                                     (make-instance 'let :def transmute-def :body
                                                    (apply #'condition-union predicates)))
                             (cons transmute-def binds))))))

(|:| #'enclose-in-lets (-> ((proper-list (cons symbol expr)) expr) expr))
(defun enclose-in-lets (binds inner)
  (iter
    (with expr = inner)
    (for def in (nreverse binds))
    (setf expr (make-instance 'let
                              :def def
                              :body expr))
    (finally (return expr))))

(defmethod parse ((match syntax:match))
  (iter
    (with sym = (gensym "match-value"))
    (with var = (make-instance 'variable
                               :name sym))
    (with expr = (make-instance 'match-exhausted))
    (for (pat . val) in-vector (syntax:arms match) downto 0)
    (for (values predicate binds) = (parse-pattern pat var))
    (setf expr
          (make-instance 'if
                         :predicate predicate
                         :then-case (enclose-in-lets binds (parse val))
                         :else-case expr))
    (finally (let* ((def (make-instance 'definition
                                        :name sym
                                        :initform (parse (syntax:val match))))
                    (let (make-instance 'let
                                        :def def
                                        :body expr)))
               (return let)))))

(defmethod parse ((funcall syntax:funcall))
  (make-instance 'funcall
                 :func (parse (syntax:func funcall))
                 :args (parse (syntax:args funcall))))

(defmethod parse ((lambda syntax:lambda))
  (make-instance 'lambda
                 :bindings (make-array (length (syntax:bindings lambda))
                                       :element-type 'symbol
                                       :initial-contents (syntax:bindings lambda))
                 :body (transform-implicit-progn (syntax:body lambda))))

(defmethod parse ((let syntax:let))
  "transform (let ((a b) (c d)) e f) into (let a b (let c d (progn e f)))"
  (flet ((let-from-definition (definition body)
           (make-instance 'let
                          :def (parse-definition definition)
                          :body body)))
    (reduce #'let-from-definition (syntax:bindings let)
            :from-end t
            :initial-value (transform-implicit-progn (syntax:body let)))))

(defmethod parse ((if syntax:if))
  "boring. recurse on `PARSE'"
  (make-instance 'if
                 :predicate (parse (syntax:predicate if))
                 :then-case (parse (syntax:then-case if))
                 :else-case (parse (syntax:else-case if))))

(defmethod parse ((primop syntax:primop))
  "boring. recurse on `PARSE'"
  (make-instance 'primop
                 :op (syntax:op primop)
                 :args (parse (syntax:args primop))))

(defmethod parse ((variable syntax:variable))
  (make-instance 'variable
                 :name (syntax:name variable)))

(defmethod parse ((quote syntax:quote))
  (make-instance 'quote
                 :it (syntax:it quote)))

(|:| #'parse-definitions (-> ((vector syntax:top-level-form)) (vector definition)))
(defun parse-definitions (vec)
  (coerce (remove nil (map 'list #'parse-definition vec))
          '(vector definition)))

(|:| #'parse-program (-> (syntax:program) program))
(defun parse-program (program
                      &aux (*types* (adjustable-vector type)))
  "transform a `SYNTAX:PROGRAM' into an `IR1:EXPR'"
  (make-instance 'program
                 :types *types*
                 :definitions (parse-definitions
                               (syntax:definitions program))
                 :entry (parse (syntax:entry program))))
