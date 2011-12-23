;;;; turn.lisp

(in-package #:mum)

(defclass turn ()
  ((actions
    :accessor actions
    :initarg :actions
    :initform nil
    :type list
    :documentation "Alist of players to action futures.  Stored in reverse
order of turn priority (i.e., the first player in this list goes last).")
   (arena
    :accessor arena
    :initarg :arena
    :type arena)
   (done-p
    :accessor done-p
    :initarg :done-p
    :initform nil
    :documentation "Is this turn over?")
   (clock
    :accessor clock
    :initarg :clock
    :type (integer 0 *))
   (messages
    :accessor messages
    :initarg :messages
    :initform nil
    :type list
    :documentation "Alist of players to textual messages displayed to them
when this turn ends.  Stored in reverse order (last message displayed is first in list).")
   (players
    :accessor players
    :initarg :players
    :initform nil
    :type list
    :documentation "Players participating in this turn.")
    ))

(defmethod print-object ((turn turn) stream)
  (print-unreadable-object (turn stream :type t :identity t)
    (format stream "~S ~S ~%  ~S" (arena turn) (clock turn)
	    (actions turn))))

(defvar *all-turns*
  nil
  "A list of all turns, ever, for debugging purposes.  This must go
  away so that they can be garbage collected at some point in the
  future.")

(defmethod initialize-instance :after ((instance turn) &rest initargs)
  (push instance *all-turns*))
  

(defun make-next-turn (turn)
  (let ((arena (arena turn))
        (clock (clock turn))
        (players (players turn)))
    (make-instance 'turn
                   :actions (make-action-slots players)
                   :arena arena
                   :clock (1+ clock)
                   :players players)))
(defun never ()
  (error "You tried to compute this future but it should be computed externally."))

(defun make-action-slot (player)
  (cons player (eager-future2:pcall 'never :lazy)))

(defun make-action-slots (players)
  (mapcar 'make-action-slot players))

(defmethod ensure-player ((turn turn) (player player) &rest ignored?)
  (the player
    (or (find player (players turn))
        (prog1 player
          (push player (players turn))
          (push (make-action-slot player)
                (actions turn))
          (setf (turns player)
                (append (turns player)
                        (list turn)))))))

(defun message-player (player turn message)
  (push (cons player message)
	(messages turn)))

(defun message-all-players (turn message)
  (mapcar (lambda (player)
	    (message-player player turn message))
	  (players turn)))

(defun finish-turn (turn)
  "Perform the actions and finish the turn.  Executed with the turn lock held."
  (let ((actions (reverse (actions turn)))
        (arena (arena turn)))
    (mapcar (lambda (player action)
              (perform-action arena player turn action))
            (mapcar #'car actions)
            (mapcar 'eager-future2:yield
                    (mapcar #'cdr actions)))
    (message-all-players turn (format nil "Turn ~D ends!" (clock turn)))
    (let ((next-turn (make-next-turn turn)))
      (setf (done-p turn) t
            (turn arena) next-turn)
      (mapcar (lambda (player)
                (setf (turns player)
                      (append (turns player)
                              (list next-turn))))
              (players turn)))))

(defun take-turn (turn player action)
  (bordeaux-threads:with-lock-held ((turn-lock *world*))
    (let ((turn-action (cdr (assoc player (actions turn)))))
      (eager-future2:force turn-action action)
      (when (and (not (done-p turn))
		 (every 'eager-future2:ready-to-yield?
                   (mapcar #'cdr
                           (actions turn))))
        (finish-turn turn)))))

