;;;; icon.lisp

(in-package #:mum)

(defclass icon ()
  ((color
    :accessor color
    :initarg :color
    :initform "white"
    :documentation "Icon color as CSS string")
   (glyph
   :accessor glyph
   :initarg :glyph)
   (tooltip
    :accessor tooltip
    :initarg :tooltip)))

(defparameter +closed-door-icon+
  (make-instance 'icon :glyph "+" :tooltip "a closed door"))

(defparameter +open-door-icon+
  (make-instance 'icon :glyph "'" :tooltip "an open door"))

(defparameter +player-icon+
  (make-instance 'icon :glyph "p" :tooltip "somebody"))

(defparameter +wall-icon+
  (make-instance 'icon :glyph "#" :tooltip "a wall"))

(defparameter +rubble-icon+
  (make-instance 'icon :glyph ";" :tooltip "a pile of rocks"))

(defun string-hash-color (string)
  (let ((result-gbr (make-array 3 :initial-element 128)))
    (loop for c across string
	 for stim in (alexandria:circular-list 0 1 2)
	 for index from 0
	 do (let ((val (digit-char-p c 36)))
	      (when val
		(let*((acc (aref result-gbr stim))
		      (room (- 256.0 acc))
		      (inc (/ room 36.0))
		      (del (- (* val inc)
			      (/ room 2.0))))
		  (setf (aref result-gbr stim)
			(+ acc del))))))
    (loop for e across result-gbr
       collect (floor e))))

(defun string-hash-color-rgb (string)
  (let ((gbr (string-hash-color string)))
    (format nil "rgb(~D,~D,~D)"
	    (elt gbr 2)
	    (elt gbr 0)
	    (elt gbr 1))))
			 
			 
  
