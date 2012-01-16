;;;; dungeon.lisp

(in-package #:mum)

(defun bounding-box (positions)
  (list (reduce #'min positions :key (compose #'first #'coordinates))
	(reduce #'min positions :key (compose #'second #'coordinates))
	(reduce #'min positions :key (compose #'third #'coordinates))
	(reduce #'max positions :key (compose #'first #'coordinates))
	(reduce #'max positions :key (compose #'second #'coordinates))
	(reduce #'max positions :key (compose #'third #'coordinates))))

(defclass dropping (icon-mixin)
  ((icon
    :initform (make-instance 'icon :glyph "," :tooltip "poo"))
   (name
    :initform "some poo")))

(defmethod collide ((player player) (poo dropping) direction (turn turn))
  (message-player player turn (format nil "You step in ~A!" (name poo))))

(declaim (special *turtles*))

(defun forward (turtle)
  (setf (coordinates turtle)
	(mapcar #'+
		(coordinates turtle)
		(pointing turtle))))
(defun fork (turtle)
  (push (make-instance 'turtle
		       :coordinates (coordinates turtle)
		       :pointing (pointing turtle)
		       :choices (choices turtle))
	*turtles*))

(defun die (turtle)
  (setf *turtles*
	(remove turtle *turtles*)))

; 0, -1| 1, 0
;  ^   |
;  .   | .>
;      |
; 1, 0 | -1, 0
;      |
;  .>  | .
;      | V

(defun right (turtle)
  "I really have no idea if this does what it says."
  (let*((pointing (pointing turtle))
	(x (first pointing))
	(y (second pointing))
	(z (third pointing)))
    (setf (pointing turtle)
	  (list
	   (if (> y 0)
	       y
	       (- y))
	   (if (> x 0)
	       (- x)
	       x)
	   z))))
	  
(defun left (turtle)
  "Two wrongs don't make a right, but three rights make a left."
  (right turtle)
  (right turtle)
  (right turtle))

(defvar *choices* `((50 ,#'forward)
		    (20 ,#'left)
		    (20 ,#'right)
		    (1 ,#'fork)
		    (1 ,#'die)))

(defclass turtle (position-mixin)
  ((dungeon
    :accessor dungeon
    :initarg :dungeon)
   (pointing
    :initarg :pointing
    :accessor pointing
    :initform (cdr (assoc :N *directions*)))
   (choices
    :accessor choices
    :initarg :choices
    :initform *choices*)))

(defclass dungeon (arena)
  ())

;; written by me then fixed by looking at
;; http://nklein.com/2009/05/code-clarity-revisited/
(defun weighted-random-choice (choices)
  (if (zerop (length choices))
      nil
      (let*((total (reduce '+ choices :key 'car))
            (r (random total)))
        (loop for x in choices
	   if (minusp (decf r (car x)))
	   return (second x)))))


(defun run-turtles (dungeon)
  (let ((*turtles* (list (make-instance 'turtle :coordinates (list (random 80) (random 24) 0)))))
    ; (print *turtles*)
    (loop while *turtles*
       ; do (print *turtles*)
       do (mapcar
	   (lambda (turtle)
	     (push (make-instance 'dropping :coordinates (coordinates turtle))
		   (entities dungeon))
	     (funcall (weighted-random-choice (choices turtle))
		      turtle))
	   *turtles*))))

(defmethod initialize-instance :after ((dungeon dungeon) &key &allow-other-keys)
  (run-turtles dungeon))
    
(defun make-valhalla ()
  (let ((valhalla (make-instance 'dungeon
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

