; Birdcage server

; To run:
; arc> (load "~/myblog.arc")
; arc> (bsv)
; go to http://localhost:8080/blog

(= postdir* "~/arc/posts/"  maxid* 0  posts* (table))
(= blogtitle* "John's Customized Blog with Mighty reload feature")

(deftem post 
  id     nil
  title  nil
  text   nil)

(def load-posts ()
  (each id (map [coerce _ 'int] (dir postdir*))
    (= maxid*      (max maxid* id)
       (posts* id) (temload 'post (string postdir* id)))))

(def save-post (p)
  (save-table p (string postdir* (p 'id))))

(def post (id) (posts* (errsafe (coerce id 'int))))

(mac blogpage body
  `(whitepage 
     (center
       (widtable 600 
         (tag b (link blogtitle* "blog"))
         (br 3)
         ,@body
         (br 3)
         (w/bars (link "archive")
                 (link "new post" "newpost"))))))

(defop viewpost req
  (aif (post (arg req "id")) 
       (post-page (get-user req) it) 
       (notfound)))

(def permalink (p) (string "viewpost?id=" (p 'id)))

(def post-page (user p) (blogpage (display-post user p)))

(def display-post (user p)
  (tag b (link (p 'title) (permalink p)))
  (when user
    (sp)
    (link "[edit]" (string "editpost?id=" (p 'id))))
  (br2)
  (pr (p 'text)))

(def notfound ()
  (blogpage (pr "No such post.")))

(defopl newpost req
  (whitepage
    (aform (fn (req)
             (let user (get-user req)
               (post-page user
                          (addpost user (arg req "t") (arg req "b")))))
      (tab
        (row "title" (input "t" "" 60))
        (row "text"  (textarea "b" 10 80))
        (row ""      (submit))))))

(def addpost (user title text)
  (let p (inst 'post 'id (++ maxid*) 'title title 'text text)
    (save-post p)
    (= (posts* (p 'id)) p)))

(defopl editpost req
  (aif (post (arg req "id"))
       (edit-page (get-user req) it)
       (notfound)))

(def edit-page (user p)
  (whitepage
    (vars-form user
               `((string title ,(p 'title) t t)
                 (text   text  ,(p 'text)  t t))
               (fn (name val) (= (p name) val))
               (fn () (save-post p)
                      (post-page user p)))))

(defop archive req
  (blogpage
    (tag ul
      (each p (map post (rev (range 1 maxid*)))
        (tag li (link (p 'title) (permalink p)))))))

(defop blog req
  (let user (get-user req)
    (blogpage
      (for i 0 4
        (awhen (posts* (- maxid* i)) 
          (display-post user it)
          (br 3))))))

(def reload-check (paths (o mtimes (table)))
		  (sleep 3)
		  (each p paths
		    (if (no (mtimes p))
			(= (mtimes p) (mtime p))
			(isnt (mtimes p) (mtime p))
			(do
			  (= (mtimes p) (mtime p))
			  (prn "reloading: " p)
			  (tostring (load p))
  			  (load-posts))))
		  (reload-check paths mtimes))


(def csv ()
	 ;(prn "starting reloading thread")
	 ;(prn (thread (reload-check '("~/myblog.arc"))))
	 (prn "starting blog server")
	 (prn (thread (asv))))

(def bsv ()
  (ensure-dir postdir*)
  (load-posts)
  (csv))

;;;;;blether below here


;;birdcage object

(= birdcagefile "~/birdcage")
(= birdcage (safe-load-table birdcagefile))

(= chestertoncurryusers (sort < '("jaspden" "cmetcalfe" "twatt" "jhoward" "atwigg" "jtidy" "asouthgate" "csmith" "ithompson" "cwood" "ocrabbe" "robbie" "cbraithwaite")))
(= chestertoncurrydates '("12/08/2007" "13/08/2007" "14/08/2007" "15/08/2007"))

(def validuser? (u) (mem u chestertoncurryusers))
(def validdate? (d) (mem d chestertoncurrydates))

(def tokey (x y) (+ x "~" y))

(def birdcageset (user date val)
		 (when (and (validuser? user) (validdate? date))
		       (= (birdcage (tokey user date)) val)
		       (save-table birdcage birdcagefile)))

(def birdcageget (user date)  (birdcage (tokey user date)))


(sort (fn (a b) (< (cadr a) (cadr b))) (map [tokens _ #\~] (keys birdcage)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    



(defop birdcage req
  (birdcagepage
    (birdcagetable  chestertoncurryusers chestertoncurrydates )))

(def birdcagesetfromreq (req val)
			(with (user  (arg req "user") date (arg req "date"))
			      (birdcageset user date val)))
(defop join req
  (pr req)
  (birdcagesetfromreq req t)
  (birdcagepage
    (birdcagetable chestertoncurryusers chestertoncurrydates )))

(defop leave req
  (pr req)
  (birdcagesetfromreq req nil)
  (birdcagepage
    (birdcagetable chestertoncurryusers chestertoncurrydates )))

(mac birdcagepage body
  `(whitepage 
     (center
       (widtable 600 
         (tag b (link "Chesterton RC curry organiser " "birdcage"))
	 (prn "hello:  " (get-user req))
         (br 3)
         ,@body
         (br 3)
         (w/bars (link "login")
                 (link "logout"))))))


;;entries as hyperlinks
(def bcentry (entry linktext linkurl user date) 
		(link entry (+ 
				 linkurl "?user=" (string user) 
				 "&date=" (string date) "")))

(def -entry (user date) 
	    (bcentry "-" "join" "join" user date))

(def +entry (user date)
	    (bcentry "X" "leave" "leave" user date))

;;entries as buttons sending post requests
(def +entry (user date)
	(prn "X")
	(form "/leave"
	      (submit "X")
	      (prn "<input type=hidden value=" user " name=user>")
              (prn "<input type=hidden value=" date " name=date>")              
	      ))

(def -entry (user date)
	(prn "-")
	(form "/join"
	      (submit "-")
	      (prn "<input type=hidden value=" user " name=user>")
              (prn "<input type=hidden value=" date " name=date>")              
	      ))

(def titlerow (users)
	      (tr
		(td)
		(each user users
		  (td user))))

(def cagerow (date users)
	     (tr
	       (td date)
	       (each user users
		 (td
		   (if (birdcageget user date)
		       (+entry user date)
		       (-entry user date)       )))))

(def birdcagetable (users dates)
		   (tag  (table border 1)
			 (titlerow users)
			(each date dates
				   (cagerow date users))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






(defop debug req
  (whitepage
    (pr "debugging page")
    (tag li (pr "req:" req))
    (tag li (pr "from user: " (get-user req )))))

(defop  arcdocs req
  (= last-req req)
  (whitepage
    (pr "req: " req)
    (para)
    (prn "Hello World")
    (link "click") 
    (prn " for") (underline (pr "more ")) (pr "stuff")
  (tag (p style "mystyle" ) (pr "Content") (pr "More Content"))))


(defop userlink req
  (userlink "arc" "click here" (prn "thanks for clicking")))

(defop mylogin req 
  (login-page 'both "Hello"
		     (fn (user ip) (prn "Welcome " user " from ip: "ip))))

(defopl example req
  (let user (get-user req)
    (uform user req (prn "Hello " user)
		    (prn "This is the example page.") (submit))))

(defopl urexample req
  (let user (get-user req)
	    (urform user req
	      (do 
		(prn "Set-cookie: mycook=" (alref (req 'args) "foo")) 
		"uexample")
	      (prn "Enter value:") 
	      (input 'foo)
	      (submit))))

(defopl uexample req 
  (prn "User " (get-user req))
  (br)
  (prn "Cookies " (req 'cooks)))


(defopl vars-form req
  (vars-form (get-user req)
	     '((int field1 42 t t "Enter int:")
	       (toks field2 (a b c) t t)
	       (string nil "bar" t nil "Can't touch this."))
	(fn (name val) (prn name " " val) (br))
	(fn () (prn "Done!"))
	"Doit"))

(defop markdown req
  (prn (markdown "Text\n\n Code\n http://arcfn.com, and *stuff*")))


(defop good-logins* req
  (prn good-logins*))


(defop-raw raw (str req) (w/stdout str
			   (prn "Set-Cookie: mycookie=42")
			   (prn)
			   (tag html
			     (tag body
			       (prn req) (br)
			       (prn (req 'ip)) (br)
			       (prn (req 'cooks)) (br)
			       (prn (req 'args))(br)
			       ))))

(defop myform req
  (form "myhandler" (single-input "Enter:" 'foo 10 "Submit")))

(defop myhandler req (prn "You entered") (prbold (alref (req 'args) "foo")))

