;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defun yo()
  "Yo"
  (interactive)
  (let((end (point))
       (beg (save-excursion
              (backward-list 1)
              (point)))
       (edit-buffer (current-buffer))
       (lisp-buffer (get-buffer "*slime-repl clojure*"))
       (eval-command 'slime-repl-return))
  (progn
     (pop-to-buffer lisp-buffer)
     (end-of-buffer)
     (pop-to-buffer edit-buffer)
     (append-to-buffer lisp-buffer beg end)
     (pop-to-buffer lisp-buffer)
     (funcall eval-command)
     (pop-to-buffer edit-buffer))))

(define-key global-map (kbd "C-x C-r") 'yo)

ilisp-buffer
(member* "*slime-repl clojure*" (buffer-list) :key #'buffer-name :test #'equal)


(defun copy-eval-last-sexp ()
  "Evaluate the last s-expression in the buffer in the Lisp listener."
  (interactive)
  (let ((end (point))
	(beg (save-excursion
	       (backward-list 1)
	       (point)))
	(edit-buffer (current-buffer))
	(lisp-buffer nil)
	(eval-command nil))
    (cond ((and (or (equal mode-name "Lisp")
		    (equal mode-name "Info"))
		(boundp 'ilisp-buffer)
		(member* ilisp-buffer (buffer-list)

			 :key #'buffer-name
			 :test #'equal)
		(get-buffer-process (get-buffer ilisp-buffer)))
	   (setq lisp-buffer (get-buffer ilisp-buffer)
		 eval-command 'return-ilisp))
	  ((and (or (equal mode-name "Common Lisp")
		    (equal mode-name "Info"))
		(boundp 'fi:common-lisp-buffer-name)
		(member* fi:common-lisp-buffer-name (buffer-list)
			 :key #'buffer-name
			 :test #'equal)
		(get-buffer-process (get-buffer fi:common-lisp-buffer-name)))
	   (setq lisp-buffer (get-buffer fi:common-lisp-buffer-name)
		 eval-command 'fi:inferior-lisp-newline))
	  ((and (or (equal mode-name "Emacs-Lisp")
		    (equal mode-name "Emacs Lisp")
		    (equal mode-name "Info"))
		(member* "*scratch*" (buffer-list)
			 :key #'buffer-name
			 :test #'equal))
	   (setq lisp-buffer "*scratch*"
		 eval-command 'eval-print-last-sexp))
	  (t nil))
    (if eval-command
	(progn
	  (pop-to-buffer lisp-buffer)
	  (end-of-buffer)
	  (other-window 1)
	  (append-to-buffer lisp-buffer beg end)
	  (pop-to-buffer lisp-buffer)
	  (funcall eval-command)
	  (other-window 1)))))
