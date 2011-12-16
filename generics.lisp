;;;; generics.lisp

(in-package #:mum)

(defgeneric ensure-player (container player-designator
                           &rest etc &key &allow-other-keys)
  ; TODO doc
)
