;;;; icon.lisp

(in-package #:mum)

(defclass icon ()
  ((glyph
   :accessor glyph
   :initarg :glyph)
   (tooltip
    :accessor tooltip
    :initarg :tooltip)))

(defparameter +player-icon+
  (make-instance 'icon :glyph "p" :tooltip "somebody"))
