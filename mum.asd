;;;; mum.asd

(asdf:defsystem #:mum
  :depends-on (
               #:alexandria
               #:cl-json
               #:cl-who
               #:eager-future2
               #:hunchentoot
               #:parenscript
               #:ssmt
               #:bordeaux-threads
               )
  :components ((:file "package")
               (:file "macros" :depends-on ("package"))
               (:file "mum" :depends-on ("package" "macros" "world"))
               (:file "action")
               (:file "icon")
               (:file "position-mixin")
               (:file "turn" :depends-on ("package" "player"))
               (:file "player" :depends-on ("package" "position-mixin"))
               (:file "world" :depends-on ("package" "turn"))
               ))

