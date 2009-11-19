(defvar *db* nil)

(defun make-cd (title artist rating ripped)
	   (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd) (push cd *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped [y/n]")))

(defun printcd (cd)
  (format t "~{~a:~a ~}~%" cd))

(defun printdb (db)
  (dolist (cd db)
    (printcd cd)))

(defun dump-db ()
  (printdb *db*))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
    (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;(defun where (&key title artist rating (ripped nil ripped-p))
;  #'(lambda (cd)
;      (and
;        (if title (equal (getf cd :title) title) t)
;        (if artist (equal (getf cd :artist) artist) t)
;        (if rating (equal (getf cd :rating) rating) t)
;        (if ripped-p (equal (getf cd :ripped) ripped) t))))


;(defun make-comparison-expr (field value)
;  (list 'equal (list 'getf 'cd field) value))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))


(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
       (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title (setf (getf row :title) title))
               (if artist (setf (getf row :artist) artist))
               (if rating (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 7 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))
(add-record (make-cd "Blood on the Tracks" "Bob Dylan" 11 t))
(add-record (make-cd "Bringing it all Back Home" "Bob Dylan" 10 nil))


;(load-db "doom.db")
;(dump-db)
;(printdb (select (where :artist "Dixie Chicks")))


