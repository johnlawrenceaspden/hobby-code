(defun new-union (lsta lstb)
  (if (null lstb) lsta
      (let ((new (car lstb)))
	    (if (member new lsta)
		(new-union lsta (cdr lstb))
		(new-union (append lsta (list new)) (cdr lstb))))))))

(defun occs (assoclst lst)
  (if (null lst) assoclst
      (let ((nval (car lst)))
	(let ((counter (assoc nval assoclst)))
	  (if (null counter)
	      (occs (append assoclst (list (cons nval 1))) (cdr lst))
	      (progn
		(setf (cdr counter) (+ 1 (cdr counter)))
		(occs assoclst (cdr lst))))))))

(defun occurrences (lst)
  (let ((occslst (occs '() lst)))
    (sort occslst (lambda (a b) (> (cdr a) (cdr b) )))))

(defun pos+recursive (lst)
  (labels ((pos+rec (lst counter)
    (if (null lst) nil
	(cons (+ (car lst) counter) (pos+rec (cdr lst) (+ counter 1))))))
  (pos+rec lst 0)))

(defun pos+iterative (lst)
  (let ((acc nil))
    (do ((l lst (cdr l))
	 (count 0 (+ count 1)))
	((null l) 'done)
      (setf acc (append acc (list (+ (car l) count)))))
    acc))

(defun pos+mapcar (lst)
  (mapcar #'+ lst (intlist (+(length lst) 1) 0)))

(defun intlist (n i)
  (if (= n i) nil
      (cons i (intlist n (+ i 1)))))

(defun pos+iterative (lst)
  (labels ((pos+it (lst count result)
	     (if (null lst) result
		 (pos+it (cdr lst) (+ count 1) (append result (list (+ count (car lst))))))))
    (pos+it lst 0 '())))

(pos+iterative '(1 2 3 4))


(defun showdots (lst)
  (if (atom lst) lst
      (format nil "(~A . ~A)" (showdots (car lst)) (showdots (cdr lst)))))

(showdots '((a b) b c))


