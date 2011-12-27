;;;; dice.lisp

(in-package #:mum)

(defclass dice ()
  ())

(defmethod name ((dice dice))
  "mystery dice")

(defclass xdy (dice)
  ((number-of-dice
    :accessor number-of-dice
    :initarg :number-of-dice
    :initarg :x)
   (number-of-sides
    :accessor number-of-sides
    :initarg :number-of-sides
    :initarg :y)))

(defmethod name ((dice xdy))
  (format nil "~Dd~D" (number-of-dice dice) (number-of-sides dice)))

(defmethod roll ((xdy xdy))
  (loop repeat (number-of-dice xdy)
       summing (1+ (random (number-of-sides xdy)))))
