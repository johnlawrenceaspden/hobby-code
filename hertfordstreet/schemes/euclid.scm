;(define (GCDlineiter count a n)
;  (display (GCD count a))
;  (display " ")
;  (if (< count n) (GCDlineiter(+ 1 count) a n) (newline) ))
;
;(define (GCDline a n) (GCDlineiter 1 a n))
;
;(define (GCDtableiter count n)
;  (GCDline count n)
;  (if (< count n) (GCDtableiter (+ 1 count) n)(newline)))
;
;(define (GCDtable n) (GCDtableiter 1 n)) 
;
;(define GCD (lambda (a b) (begin
;                            (cond
;                              ((< a b) (GCD b a))
;                              ((= b 0) a)
;                              (else (begin 
;                                      (display a)
;                                      (display " " )
;                                      (display b)
;                                      (newline)
;                                      (GCD (remainder a b) b)))))))   
;
;(define (GCD a b) (cond
;                    ((< a b)(GCD b a))
;                    ((= b 0) a)
;                    (else (GCD (remainder a b) b))))

(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (remainder a b))))
;applicative order evaluates remainder 4 times 
(GCD 206 40) 
(GCD 40 (remainder 206 40))
(GCD 40 6 ) ;remainder
(GCD 6 (remainder 40 6))
(GCD 6 4) ;remainder
(GCD 4 (remainder 6 4))
(GCD 4 2) ;remainder
(GCD 2 (remainder 4 2))
(GCD 2 0) ;remainder
2 
;normal order evaluates remainder 18 times
(GCD 206 40)
(GCD 40 (remainder 206 40))
(if (= (remainder 206 40) 0) 40 (GCD (remainder 206 40) (remainder 40 (remainder 206 40)))) ;remainder
(GCD (remainder 206 40) (remainder 40 (remainder 206 40)))
(if (= (remainder 40 (remainder 206 40)) 0 ) (remainder 206 40) (GCD (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(GCD (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) ;remainder x 2
(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) (remainder 40 (remainder 206 40)) (GCD (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
(GCD (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) ; remainder x 4
(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
(GCD (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
(remainder (remainder 206 40) (remainder 40 (remainder 206 40))); remainder x 7
2; remainder x 4 

;sequence 0 1 2 4 7 14 22 37 
;iterative 4 ; normal 0+1+2+4+7+4
;iterative 5 ; normal 0+1+2+4+7+14+7 etc 


;conclude that normal order euclid algorithm goes like exp(log n) ie O(n) for worst cases
 













