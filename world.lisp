;;;; world.lisp

(in-package #:mum)

(defclass world ()
  ((clock
    :accessor clock
    :initarg :clock
    :initform 0
    :type (integer 0 *))
   (turn-lock
    :accessor turn-lock
    :initarg :turn-lock
    :initform (bordeaux-threads:make-lock)
    :documentation "Lock for turns and current turn.")
   (turns
    :accessor turns
    :initarg :turns
    :initform (list (make-instance 'turn))
    :type list
    :documentation "A list of turns.  The last
turn in the list is the turn currently being taken.
If there is more than one turn in the list, there will be two, and the
first turn in the list is the previous turn.
Turns are destructively added to the end of this list, and players have
their own list of turns they have not received yet, which will either be
the same as this one or have it as a tail.")
   (players
    :accessor players
    :initarg :players
    :initform nil
    :type list)
   ))

(defvar *world*
  (make-instance 'world))

(defun current-turn (world)
  (car (turns world)))

(defun ensure-player (world name &rest initargs &key &allow-other-keys )
  (let ((player
         (or (find name (players world) :key 'name :test #'string-equal)
             (first (push (apply 'make-instance 'player
                                 :name name
                                 :turns (turns world)
                                 initargs)
                          (players world))))))
    (ensure-player-turn-action (current-turn world) player)
    player))

(defun take-turn (world player action)
  (bordeaux-threads:with-lock-held ((turn-lock world))
    (let ((turn-action (cdr (assoc player (actions (current-turn world))))))
      (eager-future2:force turn-action action)
      (when (every 'eager-future2:ready-to-yield?
                   (mapcar #'cdr
                           (actions (current-turn world))))
        (finish-turn world)))))

(defun finish-turn (world)
  "Perform the actions and finish the turn.  Executed with the turn lock held."
  (let*((turn (current-turn world))
        (actions (reverse (actions turn))))
    (mapcar (lambda (player action)
              (perform-action world player turn action))
            (mapcar #'car actions)
            (mapcar 'eager-future2:yield
                    (mapcar #'cdr actions)))
    (setf (done-p turn) t
          (cdr (turns world)) (list
                               (make-instance 'turn
                                              :clock (incf (clock world))
                                              :players (players turn)
                                              :actions (mapcar 'make-action-slot
                                                               (mapcar #'car
                                                                       (actions turn)))))
          (turns world) (cdr (turns world)))))
