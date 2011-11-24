;;;; position-mixin.lisp

(in-package #:mum)

(defclass position-mixin ()
  ((coordinates
   :accessor coordinates
   :initarg :coordinates)))
