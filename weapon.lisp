;;;; weapon.lisp

(in-package #:mum)

(defclass weapon (damage-mixin)
  ((name
    :accessor name
    :initarg :name)
   (damage
    :accessor damage
    :initarg :damage
    :documentation "Dice determining damage on a successful hit.")))

(defclass puny-fists (weapon)
  ((name
    :initform "puny fists")
   (damage
    :initform (make-instance 'xdy :x 1 :y 2))))
    
