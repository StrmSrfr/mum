;;;; icon.lisp

(in-package #:mum)

(defmacro who-string (&body cl-who-form)
  `(cl-who:with-html-output-to-string (,(gensym))
     ,@cl-who-form))
