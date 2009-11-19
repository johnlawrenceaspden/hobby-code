(define fresh-name
     (let ((n 0))       
       (lambda ()
         (set! n (+ n 1))
         (string->symbol (string-append "x" (number->string n))))))

(define (emit-* x y)
  (cond ((or (eqv? x 0) (eqv? y 0)) 0)
        ((eqv? x 1) y)
        ((eqv? y 1) x)
        (else (emit-binary-op * '* x y))))

(define (emit-+ x y)
  (cond ((eqv? x 0) y)
        ((eqv? y 0) x)
        (else (emit-binary-op + '+ x y))))

(define (emit-binary-op op op-name x y)
  (cond ((and (number? x) (number? y))
         (op x y))
        ((and (equal? x y)
              (not (symbol? x)))
         `(let ((y ,x))
            (,op-name y y)))
        (else
         `(,op-name ,x ,y))))

(define (with-cse receiver)
  (let ((bindings '()))
    
    (define (cseify emitter)
      (lambda operands
        (let ((exp (apply emitter operands)))
          (cond ((or (symbol? exp) (number? exp)) exp)
                ((assoc exp bindings) => cadr)
                (else (let ((name (fresh-name)))
                        (set! bindings (cons (list exp name) bindings))
                        name))))))
    
    (let ((exp (receiver cseify)))
      `(let* ,(reverse (map reverse bindings))
         ,exp))))

(define (emit-poly-value coeffs x)
  (with-cse
   (lambda (cseify)
     (let ((* (cseify emit-*))
           (+ (cseify emit-+)))
       (foldl (lambda (coeff value)
                (+ (* value x) coeff))
              0
              coeffs)))))

(emit-poly-value '(5 0 1) 'x)

(define bindings '())

(define (cseify emitter)
  (lambda operands
    (let ((exp (apply emitter operands)))
      (cond ((or (symbol? exp) (number? exp)) exp)
            ((assoc exp bindings) => cadr)
            (else (let ((name (fresh-name)))
                    (set! bindings (cons (list exp name) bindings))
                    name))))))

(define (with-cse receiver) 
  
  
  (let ((exp (receiver cseify)))
    `(let* ,(reverse (map reverse bindings))
       ,exp)))

(define (emit-sqsq x)
  (with-cse
   (lambda (cseify)
     (let ((* (cseify emit-*))
           (+ (cseify emit-+)))
       (* (+ x x) (* x x))))))



