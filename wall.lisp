;;;; wall.lisp

(in-package #:mum)

(defclass wall (icon-mixin)
  ((name; inherited from icon-mixin
    :initform "a wall")
   (icon; inherited from icon-mixin
    :initform +wall-icon+)))
