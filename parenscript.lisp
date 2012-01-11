;;;; parenscript.lisp

(in-package #:mum)

(hunchentoot:define-easy-handler (ps-lisp-library :uri "/mum/ps-lisp-library.js") ()
  (setf (hunchentoot:content-type*) "text/javascript")
  (who-string (cl-who:str (ps:ps* ps:*ps-lisp-library*))))
