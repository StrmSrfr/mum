;;;; icon.lisp

(in-package #:mum)

(defmacro define-easy-handler (name (&rest args) &rest body)
  `(hunchentoot:define-easy-handler (,name :uri ,(concatenate 'string "/mum/" (string-downcase name))) ,args
     ,@body))

(defmacro string-case (key &body cases)
  `(alexandria:switch (,key :test 'equal)
     ,@cases))

(defmacro who-string (&body cl-who-form)
  `(cl-who:with-html-output-to-string (,(gensym))
     ,@cl-who-form))
