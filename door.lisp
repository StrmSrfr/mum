;;;; door.lisp

(in-package #:mum)

(defclass door (openable)
  ((name; inherited from icon-mixin
    :initform "a closed door")
   (icon; inherited from icon-mixin
    :initform +closed-door-icon+)
   (open-p
    :accessor open-p
    :initarg :open-p
    :initform nil
    :type boolean)))

(defmethod open ((door door))
  (setf (name door) "an open door"
	(open-p door) t
	(icon door) +open-door-icon+))

(defmethod close ((door door))
  (setf (name door) "a closed door"
	(open-p door) nil
	(icon door) +closed-door-icon+))

(defmethod collide-p ((object position-mixin) (door door) (direction symbol) (turn turn))
  (and (call-next-method)
       (not (open-p door))))


