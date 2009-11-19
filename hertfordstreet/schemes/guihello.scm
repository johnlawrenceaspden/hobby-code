(require (lib "mred.ss" "mred")
         (lib "class.ss" "mzlib"))

(define the-frame
  (new frame%
       (label "Hello!")
       (width 400)
       (height 400)))

(define the-pane
  (new pane%
       (parent the-frame)))

(define the-message
  (new message%
       (label "Hello World!")
       (parent the-pane)))

(send the-frame show #t)