(require "hendersonescher/painters.ss")

(define e einstein)
(define m mark-of-zorro)
(define d diagonal-shading)
(define b black)
(define w white)
(define g gray)

(define p paint)
(define ph paint-hi-res)

(define (identity x) x)
(define rotate0 identity)



(define (corner-split painter n)
  (if (= n 0) painter
      (let((us (up-split painter (- n 1)))
           (rs (right-split painter (- n 1))))
        (right 
         (above        
          painter
          (right us us))
         (above
          (above rs rs)
          (corner-split painter (- n 1)))))))


(define (square-limit painter n)
  (let ((a (corner-split painter n)))
    ((square-of-four  flip-horiz identity
                      rotate180 flip-vert) a)))

(define ((square-of-four tl tr bl br) painter)
  (right (above (bl painter)  (tl painter)) (above (br painter) (tr painter))))


(define flipped-pairs
  (square-of-four identity flip-vert
                  identity flip-vert))

(define fourcycle (square-of-four identity rotate90 rotate270 rotate180)) 



(define ((right-push n) painter)
  (if (< n 1) 
      painter 
      (beside painter 
              (above 
               ((right-push (- n 1)) painter) 
               ((right-push (- n 1)) painter)))))


(define ((up-push n) painter)
  (if (< n 1) 
      painter 
      (above painter 
              (beside
               ((up-push (- n 1)) painter) 
               ((up-push (- n 1)) painter)))))


(define ((split f1 f2) painter n)
                        (if (= n 0)
                            painter
                            (let ((smaller ((split f1 f2) painter (- n 1))))
                              (f1 painter (f2 smaller smaller)))))


(define (above a b) (below  b a))
(define (right a b) (beside a b))
(define (left  a b) (beside b a))

(define down-split  (split below beside))
(define right-split (split right below ))
(define up-split    (split above left))
(define left-split  (split left  above))

;(paint-hi-res (square-limit  mark-of-zorro 3))
;(paint(square-limit d 4))

(paint einstein)
