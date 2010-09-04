; Birdcage server

; To run:
; arc> (load "~/birdcage.arc")
; arc> (bsv)
; go to http://localhost:8080/birdcage

;;;;; webserver startup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def bsv ()
	 (prn "starting birdcage server")
	 (prn (thread (asv))))

;;;;;;;;;;;;;;;;;birdcage object;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= birdcagefile "~/birdcage")
(= birdcage (safe-load-table birdcagefile))

(= chestertoncurryusers (sort < '("jaspden" "cmetcalfe" "twatt" "jhoward" "atwigg" "jtidy" "asouthgate" "csmith" "ithompson" "cwood" "ocrabbe" "robbie" "cbraithwaite")))
(= chestertoncurrydates '("12/08/2007" 
			  "13/08/2007" 
			"14/08/2007" 
			"15/08/2007" 
			"16/08/2007"
			"17/08/2007"
			"18/08/2007" 
			"19/08/2007"
			"20/08/2007"
			"21/08/2007" 
			"22/08/2007"
			"23/08/2007"))

(def validuser? (u) (mem u chestertoncurryusers))
(def validdate? (d) (mem d chestertoncurrydates))

(def tokey (x y) (+ x "~" y))

(def birdcageset (user date val)
		 (when (and (validuser? user) (validdate? date))
		       (= (birdcage (tokey user date)) val)
		       (save-table birdcage birdcagefile)))

(def birdcageget (user date)  (birdcage (tokey user date)))


 


(def birdcagesetfromreq (req val)
			(with (user  (arg req "user") date (arg req "date"))
                              (when (is user (get-user req))
				    (birdcageset user date val))))




(def titlerow (users)
	      (tr
		(td)
		(each user users
		  (td user))))

(def cagerow (date users)
	     (tr
	       (td date)
	       (each user users
		  (if (birdcageget user date)
		       (td (+entry user date))
		       (td (-entry user date)       )))))

(def birdcagetable (users dates)
		   (tag  (table border 1)
			 (titlerow users)
			(each date dates
				   (cagerow date users))))

(def -entry (user date) 
	    (bcentry "-" "join" user date))

(def +entry (user date)
	    (bcentry "X" "leave" user date))

;;;;;;;;;;;;;various different tries at what the page cells should look like

;;entries as hyperlinks (this is the nicest, but it uses GET to change state, which is broken)
(def bcentry (text url user date) 
		(link text (+ 
				 url "?user=" (string user) 
				 "&date=" (string date) "")))

;;entries as buttons sending post requests
(def bcentry (text url user date)
	     (form url
	       (gentag input type "submit" style "font-size:large; background-color:transparent; border:0; padding: 0;" value text)
	(prn "<input type=hidden value=" user " name=user>")
	(prn "<input type=hidden value=" date " name=date>")))

;;; some other submit button ideas.
;;; (prn "<a href = \"birdcage\" onclick=\"this.form.submit()\"> yo </a>")
;;; (submit text)     
;;; (gentag input type "submit" style "font-size:large; background:#fff none; " value text)

;;entries as hyperlinks back to the main page, but with a javascript sideeffect 
;;that sends a post request
(def bcentry (text url user date)
	     (prn 
	       "<a href=\"" "birdcage"
	       "\" onclick=\"postIt(\'" url "\',\'" user "\',\'" date "\'); 
               return false;\"> " 
	       text 
	       " </a>"))


;;;;;;;;;;;;;;;;;;;the main page;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mac birdcagepage body
  `(whitepage 
     (center
       (widtable 600 
         (tag b (link "Chesterton RC curry organiser" "birdcage"))
         (prn "<script type=\"text/javascript\" src=\"topost.js\">" "</script>")
         (br)
         (w/bars (link "login") (link "logout"))
         (br)
	 (prn "hello:  " (get-user req))
         (br 3)
         ,@body
         (br 3)))))

(defop birdcage req
  (birdcagepage
    (birdcagetable  chestertoncurryusers chestertoncurrydates )))


;;these join and leave ops smoothly redirect
(defopr join req
    (birdcagesetfromreq req t)
    (prn "birdcage"))

(defopr leave req
    (birdcagesetfromreq req nil)
    (prn "birdcage"))

;;these versions require login, and then create their own pages
(defopl join req
  (birdcagesetfromreq req t)
  (birdcagepage
    (birdcagetable chestertoncurryusers chestertoncurrydates )))

(defopl leave req
  (birdcagesetfromreq req nil)
  (birdcagepage
    (birdcagetable chestertoncurryusers chestertoncurrydates )))


;;this was an attempt to make a redirect that you needed to be logged in for.
(defopr dontwork req 
  (if (get-user req) 
      (do (birdcagesetfromreq req t)
	  (prn "birdcage")) 
      (login-page (quote both) 
		  "You need to be logged in to do that." 
		(list (fn (u ip)) (string (quote join) (reassemble-args req))))))

(defop join req
  (aif (get-user req)
       (do (login-page 'both "aargh" "arfle")
	   (prn "aargh"))
       (prn "hello")))
 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; SCRATCH SPACE BELOW HERE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;creates a page which says yo if you're logged in.
(defopl yo req (prn "yo"))

;macro expansion to see what's going on...
(macex1 '(defopl yo req (prn "yo")))

;this is the expanded version
(defop yo req
  (if (get-user req)
      (do (prn "yo"))
      (login-page 'both
	"You need to be logged in to do that."
	(list (fn (u ip))
	      (string 'yo
		(reassemble-args req))))))

;attempt at handwritten copy
(defop yowithlogin req
  (aif (get-user req)
       (do (prn "yo " it))
       (login-page 'both "yo")))


;creates a page which redirects to the hello page
(defopr index.html req (prn "hello"))
;macro expansion
(ppr (macex1 '(defopr index.html req (prn "hello"))))
;result 
(do (assert (redirector* (quote index.html)))
    (defop-raw index.html
               (g2113 req)
               (prn "hello")))


;tee hee
(defopr loop1 req (prn "loop2"))
(defopr loop2 req (prn "loop1"))









