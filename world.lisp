;;;; world.lisp

(in-package #:mum)

(defclass world ()
  ((turn-lock
    :accessor turn-lock
    :initarg :turn-lock
    :initform (bordeaux-threads:make-lock)
    :documentation "Lock for turns and current turn.")
   (players
    :accessor players
    :initarg :players
    :initform nil
    :type list)
   ))

(defvar *world*
  (make-instance 'world))

(defmethod ensure-player ((world world)
                          (name string)
                          &rest initargs &key &allow-other-keys)
  (the player
    (or (find name (players world) :key 'name :test #'string-equal)
        (let ((player
               (first (push (apply 'make-instance 'player
                                   :name name
                                   initargs)
                            (players world)))))
          (ensure-player *valhalla* player)))))

