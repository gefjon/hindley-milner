(uiop:define-package :hindley-milner/syntax/package
  (:nicknames :hm)
  (:export
   :|const| :|struct| :|enum| :|fn|

   :|funcall|
   :|lambda|
   :|let|
   :|if|

   :_
   
   :+ :- :* :/ :=

   :|boolean| :|true| :|false|

   :|fixnum|

   :|void|))
