#lang slideshow

(define (circles n)
  (map circle 
       (map (lambda(x) (* x n))
            '(1 2 3 4 5 6 7 8 9 10))))

(define (square n)
  ;pointless comment
  (filled-rectangle n n))

(define c (circle 10))
(define r (rectangle 15 20))
(define s (square 12))

(define op-symbols
  (map string->symbol
       (map (lambda (l) (string-append (car l) (cadr l) "-append"))
            '(("h" "c") ("h" "t") ("h" "b") ("v" "l") ("v" "c")("v" "r") ))))

;doesn't work in program but does interactively
;(map (lambda (s) (apply (eval s) (list c r))) op-symbols)
;(map (lambda (s) (apply (eval s) (list 10 c r c s))) op-symbols)
;(map (lambda (symbol) (apply (eval symbol) (list 10 c r c s))) op-symbols)

(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))



(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))




(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))



(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)))





(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))




(define (rgb-maker mk)
  (lambda (sz) 
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))






(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))



(provide rainbow square)


(require slideshow/flash)

(define (testflash)
  (filled-flash 30 20))

(require (planet "random.ss" ("schematics" "random.plt" 1 0)))



(require slideshow/code)



(define-syntax pict+code
  (syntax-rules ()
    ((pict+code expr)
     (hc-append 10
                expr
                (code expr)))))

(define (testmacro)
  (list
   (code (circle 10))
   (four (code (circle 10)))
   (pict+code (circle 100))))

(require scheme/class scheme/gui/base)

(define f "yo")

(define (add-drawing p)
  (let ([drawer (make-pict-drawer p)])
    (new canvas% [parent f]
         [style '(border)]
         [paint-callback (lambda (self dc)
                           (drawer dc 0 0))])))

(define (test-window)
  (set! f (new frame% [label "My Art"]
               [width 300]
               [height 300]
               [alignment '(center center)]))
  (send f show #t)
  (add-drawing (pict+code (circle 10)))
  (add-drawing (colorize (filled-flash 50 30) "yellow")))
  
(define (test)
  (list
   (series circle)
   (series square)
   (series (λ (size) (checkerboard (square size))))
   (rgb-series circle)
   (rgb-series square)
   (series (rgb-maker circle))
   (series (rgb-maker square))
   (list "red" "green" "blue")
   (list (circle 10) (square 10))
   (rainbow (square 5))
   (apply vc-append (rainbow (square 5)))
   (checkerboard (square 10))
   (checker (colorize (square 10) "red")
            (colorize (square 10) "black"))
   (four (circle 10))
   (testflash) 
   (testmacro)
   (test-window))) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  