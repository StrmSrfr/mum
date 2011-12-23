;;;; collision.lisp

(in-package #:mum)

(defmethod collide-p ((o1 position-mixin) (o2 position-mixin) (direction symbol) (turn turn))
  "Normally objects collide if they occupy the same space."
  (every #'= (coordinates o1) (coordinates o2)))

(defmethod collide ((player player) (arena arena) (direction symbol) (turn turn))
  (loop for target in (remove player (players arena)); TODO: collision with other things
     when (and (collide-p player target direction turn)
	       (collide-p target player direction turn))
     do (collide player target direction turn)))

(defmethod collide ((player player) (target player) (direction symbol) (turn turn))
  (message-player player turn (format nil "You collide with ~A!" (name target)))
  (message-player target turn (format nil "~A collides with you!" (name player)))
  (setf (coordinates player)
	(mapcar #'+
		(coordinates player)
		(mapcar (lambda (x) (* x -1))
			(cdr (assoc direction *directions*))))))
