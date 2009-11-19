(start 100 100) ;requires beginner level and the draw.ss teachpack
(draw-solid-disk (make-posn 50 50) 40 'blue)
(draw-solid-line (make-posn 15 15)
                 (make-posn 85 85) 'white)

(draw-solid-line (make-posn 50 50)
                 (make-posn 15 85) 'white)

(draw-circle (make-posn 50 50) 41 'red)