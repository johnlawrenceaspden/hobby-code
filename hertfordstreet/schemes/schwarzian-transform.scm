(define (read-directory)
  (map cdr (sort 
            (map 
             (lambda (p) (cons (file-or-directory-modify-seconds p) p)) 
             (directory-list)) 
            (lambda (p q) (< (car p) (car q))))))
