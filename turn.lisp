;;;; turn.lisp

(in-package #:mum)

(defclass turn ()
  ((actions
    :accessor actions
    :initarg :actions
    :initform nil
    :type list
    :documentation "Alist of users to action futures.  Stored in reverse
order of turn priority (i.e., the first user in this list goes last).")
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
    :documentation "Alist of users to textual messages displayed to them
when this turn ends.  Stored in reverse order (last message displayed is first in list).")
   (users
    :accessor users
    :initarg :users
    :initform nil
    :type list
    :documentation "Users participating in this turn.")
    ))

(defun never ()
  (error "You tried to compute this future but it should be computed externally."))

(defun make-action-slot (user)
  (cons user (eager-future2:pcall 'never :lazy)))

(defun ensure-user-turn-action (turn user)
  (declare (type turn turn)
           (type user user))
  (pushnew user
           (users turn)
           :key 'name
           :test 'string=)
  (or (assoc user (actions turn))
      (first (push (make-action-slot user)
                   (actions turn)))))

(defun message-all-users (turn message)
  (mapcar (lambda (user)
            (push (cons user message)
                  (messages turn)))
   (users turn)))
