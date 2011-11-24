;;;; icon.lisp

(in-package #:mum)

(defclass icon ()
  ((glyph
   :accessor glyph
   :initarg :glyph)))

(defvar +player-icon+
  (make-instance 'icon :glyph "p"))
