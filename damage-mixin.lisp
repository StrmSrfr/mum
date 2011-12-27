;;;; damage-mixin.lisp

(in-package #:mum)

(defclass damage-mixin ()
  ((hp
    :accessor hp
    :initarg :hp
    :initform (random 8)
    :documentation "Current hit (or health?) points.")
   (mhp
    :accessor mhp
    :initarg :mhp
    :initform (random 8)
    :documentation "Maximum hit (or health?) points.")))

 (defgeneric deal-damage (source target points))

(defmethod deal-damage (source (truc damage-mixin) dam)
  (message-all-players (first (last (turns truc)))
		       (format nil "~A attacks ~A!"
			       (name source)
			       (name truc)))
  (decf (hp truc) dam))

(defmethod deal-damage (source (nothing null) dam)
  ; the darkness gets attacked!
  )
