(require (lib "pretty.ss"))
(require (lib "13.ss" "srfi"))
(require (lib "1.ss" "srfi"))
(require (lib "string.ss"))

(define title 'unknown)
(define tables '())
(define scores '())

(define (read-all file)
  (let loop ()
    (let ((sexp (read file)))
      (if (not (eof-object? sexp)) (begin (parse sexp) (loop))))))

(define (parse-title sexp)
  (set! title (car sexp)))

(define (parse-table sexp)
  (set! tables 
        (cons sexp tables)))

(define (parse-erg-scores sexp)
  (set! scores (append sexp scores)))

(define (parse sexp)
  (let ((proc (string->symbol (string-append "parse-" (symbol->string (car sexp))))))
    ((eval proc) (cdr sexp))))

(define (emit)
  (pretty-print title)  (newline)
  (pretty-print tables) (newline)
  (pretty-print scores) (newline)
  )

(call-with-input-file "ergs.scm" read-all)
(emit)

(define (stringify s)
  (if (string? s) s (symbol->string s)))

(define (get-name score)
  (let ((name (car score)))
    (string-join (map stringify name))))

(define (has-cadr? s)
  (if (pair? s)
      (if (null? (cdr s)) #f #t)
      #f))

(define (is-weight? s)
  (if (has-cadr? s)
      (case (cadr s)
        ((kg lb st) #t)
        (else #f))
      #f))

(define (weight-kilos w)
  (case (cadr w)
    ((kg) (car w))
    ((lb) ( / (car w) 22/10))
    ((st) (case (length w)
            ((2) (/ ( * 14 (car w)) 22/10))
            ((4) (/ (+ ( * 14 (car w)) (caddr w)) 22/10))))
    ))

(define (get-weight-kilos score)
  (let ((weights (filter is-weight? score)))
    (if (= (length weights) 1)
        (weight-kilos (car weights))
        'error)))

(define (min->sec s)
  (if (number? s) 
      (* s 60)
      (let ((str (regexp-split ":" (stringify s))))
        (case (length str)
          ((1) (* (string->number(car str)) 60))
          ((2) (+ (* (string->number(car str)) 60) (string->number(cadr str))))
          (else 'error)))))

(define (get-erg s)
  (if (has-cadr? s)
      (case (cadr s)
        ((min) (list (min->sec (car s)) (caddr s)))
        ((m)   (list (min->sec (caddr s)) (car s)))
        (else #f))
      #f))

























#;(zip 
 (map get-name scores)
 (map get-weight-kilos scores)
 (map get-ergs scores))

#;(begin (print-struct #t)
         (define-struct table (title standard-rower-weight boat-weight oar-weight no-of-oarsmen cox-weight) #f)
         (define test (make-table 'a 'b 'c 'd 'e 'f))
         (display test))
  
  (define (di type sexp)
    (printf "~s found: ~a\n" type sexp))
  
  (define (sexp->table sexp)
    (display sexp)
    ())
  