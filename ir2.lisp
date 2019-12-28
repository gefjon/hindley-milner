(uiop:define-package :hindley-milner/ir2
    (:use :hindley-milner/defenum :trivial-types :cl)
  (:shadowing-import-from :hindley-milner/typecheck/type
                          :type)
  (:nicknames :ir2)
  (:import-from :hindley-milner/ir1)
  (:shadow
   :function :return))
(cl:in-package :hindley-milner/ir2)

(defenum expr ())

(gefjon-utils:defstruct function
  ((args (proper-list type))
   (locals (proper-list type))
   (return type)
   (body expr)))

(gefjon-utils:defstruct global
  ((type type)
   (initform expr)))

(gefjon-utils:defstruct program
  ((functions (proper-list function))
   (globals (proper-list global))))

(declaim (ftype (function (ir1:expr) program)
                parse-program))
(defun parse-program (program)
  )
