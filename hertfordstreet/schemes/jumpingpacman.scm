;;; Jumping PacMan

(define pi 3.1415926)

(define PAUSE 0.002)   ; determines the speed

;;;
;;; MODEL
;;;

;; Coordinate system

; (xmin,ymax) ... (xmax,ymax)
;    .                 .
;    .                 .
; (xmin,ymin) ... (xmax,ymin)


(define-values (xmin xmax) (values -1 1))
(define-values (ymin ymax) (values -1 1))

; At time t PacMan is at ( x(t), y(t) ).
(define (x t) (cos t))
(define (y t) (sin (* 3 t)))

;;;
;;; CONTROL
;;;

; model-coord->screen-coord : real real -> integer integer
(define (model-coord->screen-coord x y)
  ; convert the model coordinates to screen coordinates
  (values (floor (* (- WIDTH 50)  (/ (- x xmin) (- xmax xmin))))
          (floor (* (- HEIGHT 50) (/ (- y ymin) (- ymax ymin))))))

(define (control-loop)
  (let loop ([t (* -9 pi)])
    (update-view t)
    (sleep PAUSE)
    (loop (+ t .002))))

;;;
;;; VIEW
;;;

; The model is shown in a WIDTH x HEIGHT frame

; Dimensions
(define-values (WIDTH HEIGHT) (values 600 600))

; Instantiate frame and show it
(define frame (instantiate frame% 
                ("Jumping PacMan")))
(send frame show #t)

; Put a canvas inside the frame
(define my-canvas%
  (class canvas%
    (inherit get-dc)
    (define/override (on-paint)       
      (control-loop))
    (super-instantiate ())))

(define canvas (instantiate my-canvas% (frame) 
                 (min-width  WIDTH)
                 (min-height HEIGHT)))

; Make a PacMan bitmap by loading a gif file
(define pacman 
  (make-object bitmap% "PacMan2.GIF" 'gif))

; Update the view by drawing PacMan on his new position.
; Due to the white border around PacMan, we don't need
; to delete the old one, before drawing him again.
(define (update-view t)
  (let ([drawing-context (send canvas get-dc)])
    (let-values ([(sx sy) (model-coord->screen-coord (x t) (y t))])
      (send drawing-context draw-bitmap pacman sx sy 'solid))))