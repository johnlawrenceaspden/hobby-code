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

(sort (fn (a b) (< (cadr a) (cadr b))) (map [tokens _ #\~] (keys birdcage)))