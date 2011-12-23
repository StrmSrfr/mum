;;;; icon-mixin.lisp

(in-package #:mum)

(defclass icon-mixin (position-mixin)
  ((name
   :accessor name
   :initarg :name)
   (icon
    :accessor icon
    :initarg :icon)))
