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

(defun never ()
  (error "You tried to compute this future but it should be computed externally."))

(defun make-action-slot (player)
  (cons player (eager-future2:pcall 'never :lazy)))

(defun ensure-player-turn-action (turn player)
  (declare (type turn turn)
           (type player player))
  (pushnew player
           (players turn)
           :key 'name
           :test 'string=)
  (or (assoc player (actions turn))
      (first (push (make-action-slot player)
                   (actions turn)))))

(defun message-all-players (turn message)
  (mapcar (lambda (player)
            (push (cons player message)
                  (messages turn)))
   (players turn)))
