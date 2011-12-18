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

(defun player-icon-json-alist (player)
  "A helper function with a terrible name."
  `((:coordinates . ,(coordinates player))
    (:html . ,(who-string
               (:abbr :title (tooltip (icon player))
                      (cl-who:str (glyph (icon player))))))))

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
          (action-prompt action action)))))

(define-easy-handler act ()
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((player (hunchentoot:session-value 'user)))
    (handle-action player
                   (json:decode-json-from-string
                    (caar (hunchentoot:post-parameters*))))
    (json:encode-json-to-string
     `(
       ((:type . "icons")
        (:arguments .
                    ,(mapcar 'player-icon-json-alist
                             (players *world*))))
       ,@(loop while (done-p (first (turns player)))
              append (mapcar (lambda (text)
                           `((:type . "message")
                             (:arguments . (,text))))
                         (reverse
                          (mapcar #'cdr
                                 (remove player (messages (pop (turns player))) :key #'car :test-not #'eq)))))
      ((:type . "clock")
       (:arguments . (,(clock (first (last (turns player)))))))
       ))))

(define-easy-handler login (name)
  (let ((player (ensure-player *world* name)))
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
        "#viewport { background-color: black; border: black; color: white; font-family: monospace; height: 24em; width: 80em; }"
        "#viewport td { height: 1em; width: 1em; margin: 0; padding: 0; }"
        "#movebox table tr td input { height: 2.5em; width: 2.5em; } ")
      (:script :type "text/javascript"
               (cl-who:str (ps:ps* ps:*ps-lisp-library*)))
      (:script :type "text/javascript"
               (cl-who:str
                (parenscript:ps
                  (defvar *clock* (ps:lisp (clock user)))

                  (defun draw-icon (icon)
                    (when (= 0 (ps:@ icon coordinates 2))
                      (ps:chain ($ (+ "#char-"
                                      (ps:@ icon coordinates 0)
                                      "-"
                                      (ps:@ icon coordinates 1)))
                                (html (ps:@ icon html)))))

                  (defun update-handler (update old-action old-arguments old-clock)
                    "The action, arguments, and clock  are those that were taken
which initiated this update."
                    (case (ps:@ update type)
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
                      (:icons
                       ;; TODO just add them for now; later we'll have to deal with clearing the old ones
                       (mapcar #'draw-icon
                               (ps:@ update :arguments))
                       )))

                  (defun new-turn-handler (data old-action old-arguments old-clock)
                    (ps:chain $
                           (each (ps:chain *JSON*
                                        (parse data))
                                 (lambda (index value)
                                   (update-handler value old-action old-arguments old-clock)))))

                  (defun act (action args clock)
                    (ps:chain ($ ".action")
                              (attr "disabled" t))
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
                                                    false)))))))
                )))
     (:body
      (:div
       (:p "Name: "
           (cl-who:str (name
                        user)))
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
        (:form :id "movebox"
               (:table
                (:tr (:td (:input :type "button" :id "move-nw" :value "NW" :class "action"))
                     (:td (:input :type "button" :id "move-n" :value "N" :class "action"))
                     (:td (:input :type "button" :id "move-ne" :value "NE" :class "action")))
                (:tr (:td (:input :type "button" :id "move-w" :value "W" :class "action"))
                     (:td (:input :type "button" :id "stay" :value "." :class "action"))
                     (:td (:input :type "button" :id "move-e" :value "E" :class "action")))
                (:tr (:td (:input :type "button" :id "move-sw" :value "SW" :class "action"))
                     (:td (:input :type "button" :id "move-s" :value "S" :class "action"))
                     (:td (:input :type "button" :id "move-se" :value "SE" :class "action"))))))
       (:script :type "text/javascript"; TOOD: move this to a proper test suite
		(cl-who:str
		 (parenscript:ps
		   (when (not (member 1 '(1 2 3)))
		     (ps:chain document
			       (write "Attention mum installer: your parenscript has a buggy 'member'."))))))
       ))))))

