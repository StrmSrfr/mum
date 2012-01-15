;;;; arena.lisp

(in-package #:mum)

(defclass arena ()
  ((name
    :accessor name
    :initarg :name
    :documentation "Name of this arena (for humans).  Need not be unique.")
   (entities
    :accessor entities
    :initarg :entities
    :initform nil
    :type list
    :documentation "Entities (players, walls, ...) in this arena.")
   (turn
    :accessor turn
    :initarg :turn
    :initform nil
    :type (or null turn)
    :documentation "The active turn (if any) associated with this arena.")))

(defmethod print-object ((arena arena) stream)
  (print-unreadable-object (arena stream :type t :identity t)
    (format stream "~S" (name arena))))

(defmethod players ((arena arena))
  (remove-if-not (rcurry #'typep 'player)
		 (entities arena)))

(defmethod walls ((arena arena))
  (remove-if-not (rcurry #'typep 'wall)
		 (entities arena)))

(defun make-valhalla ()
  (let ((valhalla (make-instance 'arena
				:name "Valhalla")))
    (loop for y from 1 to 24
       do (loop for x from 1 to 80
	     when (or (member x '(1 80))
		      (member y '(1 24)))
	     do (push (make-instance 'wall
				     :coordinates (list x y 0))
		      (entities valhalla))))
    valhalla))

(defvar *valhalla*
  (make-valhalla))

(defmethod ensure-player ((arena arena) (player player) &rest ignored?)
  (the player
    (or (find player (entities arena))
        (let ((player (first (push player (entities arena)))))
          (unless (turn arena)
            (setf (turn arena) (make-instance 'turn
                                              :arena arena
                                              :clock 0)))
          (ensure-player (turn arena) player)))))

(defmethod delete-player ((arena arena) (player player))
  (setf (entities arena)
	(remove player (entities arena)))
  (when (null (players arena))
    (setf (turn arena)
	  nil)))
