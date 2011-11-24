;;;; user.lisp

(in-package #:mum)

(defclass user (position-mixin)
  ((name
   :accessor name
   :initarg :name)
   (icon
    :accessor icon
    :initarg icon
    :initform +player-icon+)
   (coordinates; inherited from position-mixin
    :initform (list (1+ (random 79)) (1+ (random 23)) 0))
   (turns
    :accessor turns
    :initarg :turns
    :initform nil
    :type list
    :documentation "Turns this user hasn't seen yet.")))

