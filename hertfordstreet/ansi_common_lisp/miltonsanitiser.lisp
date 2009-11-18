;;;;This is to extract the text of paradise lost from the html file
;;;;toccer-new2.html
;;;;which I found at
;;;;http://etext.virginia.edu/etcbin/toccer-new2?id=MilPL67.sgm&images=images/modeng&data=/texts/english/modeng/parsed&tag=public&part=all


(load "favourites.lisp")

;;given a file, build a list of tokens and punctuation
(defun tokenize (filename) 
  (with-open-file (file filename :direction :input)
    (let ((buffer (make-string 100))  ; words no more than 100 letters long. crash if so!
	  (pos 0)
	  (acc nil))                                                ;we will build our list in acc (note nreverse later)
      (labels ((emit-buffer ()                                           ;add whatever's in the buffer to the list
		 (progn (when (> pos 0)
			  (setf acc (cons (subseq buffer 0 pos) acc)))
			(setf pos 0)))
	       (store-char (c)                                           ;add a new character to the buffer
		 (progn
		   (setf (aref buffer pos) c)
		   (incf pos)))
	       (emit-tok (c)                                             ; add a single character to the list
		 (setf acc (cons c acc)))
	       (punctuation-char-p (c)                                   ;punctuation characters are word boundaries too
		 (if (member c '(#\. #\, #\; #\? #\! #\( #\) )) t nil))
	       (read-until-char (c)                                                          ;throw away input until....
		 (do ((a (read-char file nil 'eof)(read-char file nil 'eof)))
		     ((eql a c) 'done)))
	       )
	(do ((tok (read-char file nil 'eof) (read-char file nil 'eof)))
	    ((eql tok 'eof) (nreverse acc))
	  (cond ((eql tok #\<)(read-until-char #\>))                            ;chuck html tags
		((digit-char-p tok) (read-until-char #\:) (emit-tok #\Newline)) ;99: marks a line boundary. replace with newline.
		((lisp::whitespace-char-p tok) (emit-buffer))                   ;on whitespace print the word so far
		((punctuation-char-p tok) (emit-buffer) (emit-tok tok))         ;as whitespace but emit the char as well
		(t (store-char tok))))))))                                      ;text. add to token in buffer
 
(let* ((withpreamble (tokenize "toccer-new2.html")) ;pull in the html, which has a great heap of stuff before the poem
       (paradiselist (dbg(member "Of" withpreamble :test #'equal))))  ; the first word of the actual text is 'Of'
  (with-open-file (file "paradiselost.txt" :direction :output :if-exists :supersede)
    (dolist (x paradiselist)
      (format file "~A " x))))