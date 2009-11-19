(define herman 0.2)
(define mightyjoe 0.6)

(define (likelihood p-head results)
  (if (null? results) 1
      ( case (car results)
         ('head (* p-head (likelihood p-head (cdr results))))
         ('tail (* (- 1 p-head) (likelihood p-head (cdr results)))))))

(define results '(head head tail))

(likelihood herman results)
(likelihood mightyjoe results)

(define prior `((,herman 0.5) (,mightyjoe 0.5)))

(define (prior->likelihoods prior results)
  (if (null? prior) '()
      (cons (list (likelihood (caar prior) results) (cadar prior)) (prior->likelihoods (cdr prior) results))))

(define (sum-of-probabilities likelihoods)
  (apply + (map (lambda (l) (apply * l)) likelihoods)))

(define (likelihoods->posterior likelihoods)
  (let ((total (sum-of-probabilities likelihoods)))
    total))