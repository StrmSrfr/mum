;;;; arena.lisp

(in-package #:mum)

(defclass arena ()
  ((name
    :accessor name
    :initarg :name
    :documentation "Name of this arena (for humans).  Need not be unique.")
   (players
    :accessor players
    :initarg :players
    :initform nil
    :documentation "Players in this arena.")
   (turn
    :accessor turn
    :initarg :turn
    :initform nil
    :type (or null turn)
    :documentation "The active turn (if any) associated with this arena.")))

(defmethod print-object ((arena arena) stream)
  (print-unreadable-object (arena stream :type t :identity t)
    (format stream "~S" (name arena))))

(defvar *valhalla*
  (make-instance 'arena
                 :name "Valhalla"))

(defmethod ensure-player ((arena arena) (player player) &rest ignored?)
  (the player
    (or (find player (players arena))
        (let ((player (first (push player (players arena)))))
          (unless (turn arena)
            (setf (turn arena) (make-instance 'turn
                                              :arena arena
                                              :clock 0)))
          (ensure-player (turn arena) player)))))
