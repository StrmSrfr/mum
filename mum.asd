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
               (:file "generics" :depends-on ("package"))
               (:file "mum" :depends-on ("package" "macros" "world"))
               (:file "action" :depends-on ("package" "openable"))
               (:file "arena" :depends-on ("package" "generics" "player" "wall"))
	       (:file "collision" :depends-on ("package"))
	       (:file "damage-mixin" :depends-on ("package"))
	       (:file "dice" :depends-on ("package"))
	       (:file "door" :depends-on ("package" "openable"))
	       (:file "dungeon" :depends-on ("package" "arena"))
	       (:file "entity" :depends-on ("package" "damage-mixin" "icon-mixin"))
               (:file "icon" :depends-on ("package"))
               (:file "icon-mixin" :depends-on ("package" "icon" "position-mixin"))
	       (:file "openable" :depends-on ("icon-mixin"))
	       (:file "parenscript" :depends-on ("package"))
               (:file "position-mixin" :depends-on ("package"))
               (:file "turn" :depends-on ("package" "generics" "player"))
               (:file "player" :depends-on ("package" "entity"))
               (:file "wall" :depends-on ("package" "entity"))
               (:file "weapon" :depends-on ("package"))
               (:file "world" :depends-on ("package" "generics" "turn"))
               ))

