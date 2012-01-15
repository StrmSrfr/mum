;;;; wall.lisp

(in-package #:mum)

(defclass wall (entity)
  ((name; inherited from icon-mixin
    :initform "a wall")
   (icon; inherited from icon-mixin
    :initform +wall-icon+)))
