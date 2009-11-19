#lang scheme

(let ((f (let ((x 2)) (lambda () x))))
  (+ (let ((x 3)) (f))
     (let ((x 4)) (f))))

((lambda (f) 
   (+ (let ((x 3)) (f))
      (let ((x 4)) (f))))
 (let ((x 2)) (lambda () x)))

((lambda (f) 
   (+ ((lambda (x) (f)) 3)((lambda (x) (f)) 4)))
 (let ((x 2)) (lambda () x)))

((lambda (f)
   (* ((lambda (x) (f)) 3)((lambda (x) (f)) 4))) 
 ((lambda (x) (lambda () x)) 2))

((lambda (getx)
   ((lambda (x) (getx))
    'dynamic))
 ((lambda (x)
    (lambda () x))
  'lexical))

(let ((getx ((lambda (x)
               (lambda () x))
             'lexical)))
  ((lambda (x) (getx))
   'dynamic))

(let ((getx (let ((x 'lexical))
              (lambda () x))))
  ((lambda (x) (getx))
   'dynamic))

(let ((getx (let ((x 'lexical)) (lambda () x))))
  (let ((x 'dynamic)) (getx)))

(let ((getx (let ((x 'lexical)) (lambda () x))))
  (let ((x 'dynamic)) (getx)))

(define getx
  (let ((x 'lexical))
    (lambda () x)))

(let ((x 'dynamic))
  (getx))
























