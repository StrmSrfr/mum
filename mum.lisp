;;;; mum.lisp

(in-package #:mum)

(defvar *acceptor*
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 13013)))

(hunchentoot:define-easy-handler (index :uri "/mum/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (cl-who:with-html-output-to-string (s)
    (:html
     (:head
      (:title "mum: multi-user mum"))
     (:body
      (:div
       (:p "Welcome to mum!")
       (:p "How about a nickname to get started?")
       ((:form :method "post" :action "login")
        (:label "Name:"
                (:input :type "text" :name "name"))
        (:input :type "submit")))))
    s))

(defun icon-json-alist (icon-mixin)
  "A helper function with a terrible name."
  `((:coordinates . ,(coordinates icon-mixin))
    (:html . ,(who-string
               (:abbr :title (tooltip (icon icon-mixin))
		      :style (format nil "color: ~A" (color (icon icon-mixin)))
                      (cl-who:str (glyph (icon icon-mixin))))))))

(defun handle-action (player action-decoded-json)
  (let ((verb (cdr (assoc :action action-decoded-json)))
        (arguments (cdr (assoc :arguments action-decoded-json)))
        (clock (cdr (assoc :clock action-decoded-json))))
    (let ((action (make-action verb arguments))
          (turn (or
                 (find clock (turns player) :key 'clock :from-end t)
                 (and (zerop clock) (first (last (turns player)))))))
      (if (action-fully-specified-p action)
          (if turn
              (take-turn turn player action)
              (error "turn not found"); TODO
              )
          (action-prompt action player)))))

(define-easy-handler quit ()
  (setf (hunchentoot:content-type*) "text/html")
  (let*((user (hunchentoot:session-value 'user))
	(turn (first (last (turns user)))))
    (message-all-players turn
			 (format nil "~A is a quitter!"
				 (name user)))
    (delete-player *world* user))
  (who-string
    (:html
     (:head
      (:title "quit")
      )
     (:body
      (:p "Smell you later")))))


(define-easy-handler act ()
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((player (hunchentoot:session-value 'user)))
    (handle-action player
                   (json:decode-json-from-string
                    (caar (hunchentoot:post-parameters*))))
    (if (prompt player)
	(prog1
	    (json:encode-json-to-string
	     `(((:type . "prompt")
		(:arguments ,@(prompt player)))))
	     (setf (prompt player)
		   nil))
	(json:encode-json-to-string
	 `(
	   ((:type . "icons")
	    (:arguments .
			,(mapcar 'icon-json-alist
				 (icons (arena (first (last (turns player))))))))
	   ,@(loop while (done-p (first (turns player)))
		append (mapcar (lambda (text)
				 `((:type . "message")
				   (:arguments . (,text))))
			       (reverse
				(mapcar #'cdr
					(remove player (messages (pop (turns player))) :key #'car :test-not #'eq)))))
	   ((:type . "clock"); N.B. must be run after messages loop
	    (:arguments . (,(clock (first (turns player))))))
	   ((:type . "hp")
	    (:arguments ,(hp player)))
	   ((:type . "stragglers"); N.B. must be run after messages loop
	    (:arguments ,(mapcar 'name
				 (mapcar #'car
					 (remove-if 'eager-future2:ready-to-yield?
						    (actions (first (turns player))); waiting turn
						    :key #'cdr)))))
	   )))))

(define-easy-handler login (name)
  (let ((player (ensure-player *world* (who:escape-string name))))
    (setf (hunchentoot:session-value 'user) player)
    (hunchentoot:redirect "/mum/view")))

(hunchentoot:define-easy-handler (view :uri "/mum/view") ()
  (setf (hunchentoot:content-type*) "text/html")
  (let ((user (hunchentoot:session-value 'user)))
  (who-string
    (:html
     (:head
      (:title "mum: multi-user mum")
      (:script :type "text/javascript"
               :src "//ajax.googleapis.com/ajax/libs/jquery/1.6.0/jquery.min.js")
      (:style :type "text/css"
        "#clock { float: right }"
        "#viewport { background-color: black; border: black; color: white; font-family: monaco, monospace; font-size: 12px; height: 24em; width: 80em; }"
        "#viewport td { height: 1em; width: 1em; margin: 0; padding: 0; }"
        "#movebox table tr td input { height: 2.5em; width: 2.5em; } ")
      (:script :type "text/javascript"
               (cl-who:str (ps:ps* ps:*ps-lisp-library*)))
      (:script :type "text/javascript"
               (cl-who:str
                (parenscript:ps
                  (defvar *clock* (ps:lisp (clock user)))

		  (defvar *icons* (list))

		  (defun coordinate-id (coordinates)
		    (+ "#char-"
		       (ps:@ coordinates 0)
		       "-"
		       (ps:@ coordinates 1)))

                  (defun draw-icon (icon)
                    (when (= 0 (ps:@ icon coordinates 2))
		      (let ((id (coordinate-id (ps:@ icon coordinates))))
			(setf (ps:@ icon old-html)
			      (ps:chain ($ id)
					(html)))
			(ps:chain ($ id)
				  (html (ps:@ icon html))))))

		  (defun undraw-icon (icon)
		    (ps:chain ($ (coordinate-id (ps:@ icon coordinates)))
			      (html (ps:@ icon old-html))))

		  (defun rest (array) ; TODO CHEAP
		    (ps:chain array (slice 1)))

                  (defun update-handler (update old-action old-arguments old-clock)
                    "The action, arguments, and clock  are those that were taken
which initiated this update."
                    (case (ps:@ update type)
		      (:prompt
		       (ps:chain ($ "#messages")
				 (append
				  (ps:who-ps-html
				   (:form
				    (:span :id "prompt"
					    (ps:@ update :arguments 0))))))
		       (mapcar (lambda (choice)
				 (ps:chain ($ "#prompt")
					   (append
					    (ps:who-ps-html
					     (:input :type "button" :value choice)))))
				   (rest (ps:@ update :arguments)))
		       (ps:chain ($ "#prompt input")
				 (click (lambda ()
					  (when (null old-arguments)
					    (setf old-arguments (list)))
					  (act old-action (append old-arguments
								  (ps:chain ($ this) (val)))
					       old-clock)
					  ps:f))))
                      (:message
                       (ps:chain ($ "#messages")
                              (append (+ "<p>" (ps:@ update :arguments 0)
                                         "</p>"))))
                      (:clock
                       (let ((clock (ps:@ update :arguments 0)))
                         (if (= clock *clock*)
                           (set-timeout (lambda ()
                                          (act old-action old-arguments old-clock))
                                        1000)
                           (ps:chain ($ ".action")
                                     (attr "disabled" ps:f)))
                         (ps:chain ($ "#clock")
                                   (html clock))
                         (setf *clock* clock)))
		      (:hp
		       (ps:chain ($ "#hp")
				 (html (+ (ps:@ update :arguments 0)
					  "/"
					  (ps:lisp (mhp user))))))
		      (:stragglers
		       (let*((stragglers (ps:@ update :arguments 0))
			     (stragglers-string
			      (if (member (ps:lisp (name user)) stragglers)
				  "you!"
				  (ps:chain stragglers
					    (join ", ")))))
			 (ps:chain ($ "#stragglers")
				   (html 
				    stragglers-string))))
                      (:icons
		       (let ((icons (ps:@ update :arguments)))
			 (mapcar #'undraw-icon *icons*)
			 (setf *icons* icons)
			 (mapcar #'draw-icon icons)))
                       ))

                  (defun new-turn-handler (data old-action old-arguments old-clock)
                    (ps:chain $
                           (each (ps:chain *JSON*
                                        (parse data))
                                 (lambda (index value)
                                   (update-handler value old-action old-arguments old-clock)))))

                  (defun act (action args clock)
		    (ps:chain ($ "#talktext")
			      (val ""))
                    (ps:chain ($ ".action")
                              (attr "disabled" t))
		    (ps:chain ($ "#messages")
			      (html ""))
                    (unless clock
                      (setf clock *clock*))
                    (ps:chain $
                              (post "act"
                                    (ps:chain *JSON*
                                              (stringify
                                               (ps:create :action action
                                                          :arguments args
                                                          :clock *clock*)))
                                    (let ((old-clock *clock*))
                                      (lambda (data)
                                        (new-turn-handler data action args old-clock)))
                                    )))

                  (defun move (direction)
                    (act "move" (list direction)))

                  (defun stay ()
                    (act "stay" ()))

                  (defun talk (text)
                    (act "talk" (list text)))

                  (ps:chain ($ (ps:lambda ()
                                 (stay)
                                 (ps:chain ($ "#talkbox")
                                          (submit (lambda ()
                                                    (talk (ps:chain ($ "#talktext") (val)))
                                                    false)))
                                 (ps:chain ($ "#movebox table tr td input")
                                          (click (lambda ()
                                                    (move (ps:chain ($ this) (val)))
                                                    false)))
				 (ps:chain ($ "#spellbox input")
					   (click (lambda ()
						    (act (ps:chain ($ this) (val)) ())
						    false))))))
                ))))
     (:body
      (:div
       (:table
	(:tr
	 (:th "Name: ")
	 (:td
           (cl-who:str (name
                        user)))
	 (:th "Level: ")
	 (:td :id "level"
	      (cl-who:str (level user)))
	 (:th "HP: ")
	 (:td :id "hp")
	 (:td (:a :href "quit" "quit"))))
       (:p "Waiting on: "
	   (:span :id "stragglers"
		  "computers"))
       (:p :id "clock")
       (:div
        (:table :id "viewport"
         (loop for y from 1 to 24
              do (cl-who:htm
                  (:tr (loop for x from 1 to 80
                          do (cl-who:htm
                              (:td :id (format nil "char-~D-~D" x y)
                                   "."))))))))
       (:div :id "messages")
       (:div
        (:form :id "talkbox"
               (:input :type "text" :id "talktext" :size 60 :name "chat")
               (:input :type "submit" :value "Chat" :class "action")))
       (:div
        (:form :id "movebox" :style "float: left"
               (:table
                (:tr (:td (:input :type "button" :id "move-nw" :value "NW" :class "action"))
                     (:td (:input :type "button" :id "move-n" :value "N" :class "action"))
                     (:td (:input :type "button" :id "move-ne" :value "NE" :class "action")))
                (:tr (:td (:input :type "button" :id "move-w" :value "W" :class "action"))
                     (:td (:input :type "button" :id "stay" :value "." :class "action"))
                     (:td (:input :type "button" :id "move-e" :value "E" :class "action")))
                (:tr (:td (:input :type "button" :id "move-sw" :value "SW" :class "action"))
                     (:td (:input :type "button" :id "move-s" :value "S" :class "action"))
                     (:td (:input :type "button" :id "move-se" :value "SE" :class "action")))))
	(:form :id "spellbox" :style "float: left"
	       (:input :type "button" :id "proxiport" :value "proxiport" :class "action")
	       (:input :type "button" :id "attack" :value "attack" :class "action")
	       (:input :type "button" :id "build" :value "build" :class "action")
	       (:input :type "button" :id "open" :value "open" :class "action")
	       (:input :type "button" :id "close" :value "close" :class "action")))
       (:script :type "text/javascript"; TOOD: move this to a proper test suite
		(cl-who:str
		 (parenscript:ps
		   (when (not (member 1 '(1 2 3)))
		     (ps:chain document
			       (write "Attention mum installer: your parenscript has a buggy 'member'."))))))
       ))))))

