;;;; damage-mixin.lisp

(in-package #:mum)

(defclass damage-mixin ()
  ((hp
    :accessor hp
    :initarg :hp
    :initform (1+ (random 8))
    :documentation "Current hit (or health?) points.")
   (mhp
    :accessor mhp
    :initarg :mhp
    :initform (1+ (random 8))
    :documentation "Maximum hit (or health?) points.")))

 (defgeneric deal-damage (source target weapon points))

(defmethod deal-damage (source (truc damage-mixin) weapon dam)
  (message-all-players (first (last (turns truc)))
		       (format nil "~A attacks ~A with a ~A!"
			       (name source)
			       (name truc)
			       (name weapon)))
  (decf (hp truc) dam))

(defmethod deal-damage (source (nothing null) weapon dam)
  ; the darkness gets attacked!
  )
