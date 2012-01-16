;;;; package.lisp

(defpackage #:mum
  (:use #:cl #:parenscript)
  (:import-from #:alexandria #:compose #:curry #:rcurry)
  (:shadow close open))

