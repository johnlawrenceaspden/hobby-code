(defun compress (lst)
  (if (atom lst) lst
      (compr (car lst) 1 (cdr lst))))


(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
	(if (eql next elt)
	    (compr elt (+ n 1) (cdr lst))
	    (cons (n-elts elt n)
		  (compr (car lst) 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (= n 1) elt
      (list elt n)))

(defun uncompress (lst)
  (cond ((null lst) nil)
	((consp (car lst)) (append (nlist (caar lst) (cadar lst)) (uncompress (cdr lst))))
	(t (cons (car lst) (uncompress (cdr lst))))))

(defun nlist (elt n)
  (if (= n 0) nil
      (cons elt (nlist elt (- n 1)))))
      
      

;(uncompress (compress '(1 1 1 0 0 1 1 0 1 0 1 1 0)))

(defun palindrome (lst)
	   (let* ((len (length lst))
		  (mid (floor (/ len 2))))
	     (if (evenp len)
		 (equal (subseq lst 0 mid)
			(reverse (subseq lst mid len)))
		 (equal (subseq lst 0 mid)
			(reverse (subseq lst (+ mid 1)))))))

;(palindrome "abba")