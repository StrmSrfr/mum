;;;; player.lisp

(in-package #:mum)

(defclass player (entity)
  (;name inherited from icon-mixin
   (icon; inherited from icon-mixin
    :accessor icon
    :initarg :icon
    :initform +player-icon+)
   (coordinates; inherited from position-mixin
    :initform (list (1+ (random 79)) (1+ (random 23)) 0))
   (prompt
    :accessor prompt
    :initarg :prompt
    :initform nil
    :type list
    :documentation "The current prompt (if any).  TODO: this better")
   (weapons
    :accessor weapons
    :initarg :weapons
    :initform (list (make-instance 'puny-fists))
    :type list)
   (level
    :accessor level
    :initarg :level
    :initform 1
    :type integer)
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

(defmethod clock ((player player))
  (clock (first (last (turns player)))))

(defmethod arena ((player player))
  (arena (first (last (turns player)))))
