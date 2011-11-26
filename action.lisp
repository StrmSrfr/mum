;;;; action.lisp

(in-package #:mum)

(defclass action ()
  ((verb
    :accessor verb
    :initarg :verb
    :type symbol
    :documentation "A keyword for this action, or an uninterned symbol
representing an unrecognized action.")
   (arguments
    :accessor arguments
    :initarg :arguments
    :initform nil
    :type list))
  (:documentation "taken during a turn"))

(defparameter *action-verbs*
  '(:talk :move)
  "All recognized action verbs.")

(defun make-action (verb arguments)
  (declare (type string verb)
           (type list arguments))
  (make-instance 'action
                 :verb (or (find verb *action-verbs* :test 'string-equal)
                           (make-symbol (string-upcase verb)))
                 :arguments arguments))

(defgeneric action-fully-specified-p-2 (verb arguments))

(defun action-fully-specified-p (action)
  (action-fully-specified-p-2 (verb action) (arguments action)))

(defgeneric action-prompt-3 (player verb arguments))

(defun action-prompt (action player)
  (action-prompt-3 player (verb action) (arguments action)))

(defmethod action-prompt-3 (player verb arguments)
  "You wanna what with a what!?")

(defmethod action-fully-specified-p-2 (verb arguments)
  nil)

(defmethod action-fully-specified-p-2 ((verb (eql :talk)) (arguments list))
  "One argument: the text."
  (> (length arguments) 0))


(defgeneric perform-action-5 (world player turn verb arguments))

(defun perform-action (world player turn action)
  (perform-action-5 world player turn (verb action) (arguments action)))

(defmethod perform-action-5 (world player turn (verb (eql :talk)) (arguments list))
  (message-all-players turn (format nil "~A says \"~A\"." (name player) (first arguments))))
