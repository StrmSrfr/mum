;;;; player.lisp

(in-package #:mum)

(defclass player (position-mixin)
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
    :documentation "Turns this player hasn't seen yet.  The last turn in
the list is the turn currently being taken.")))

(defmethod print-object ((player player) stream)
  (print-unreadable-object (player stream :type t :identity t)
    (format stream "~S" (name player))))


