;;;; package.lisp

(defpackage #:mum
  (:use #:cl #:parenscript)
  (:import-from #:alexandria #:curry #:rcurry)
  (:shadow close open))

