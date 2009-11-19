(require (lib "mred.ss" "mred")
         (lib "class.ss" "mzlib"))

(define frame
  (new frame%
       (label "Hello!")
       (width 400)
       (height 400)))


(define panel (instantiate horizontal-panel% (frame)))
    
    (instantiate button% ("Pause" panel (lambda (button event) (sleep 5))))
    
    (instantiate button% () (label "click me") (parent panel)
      (callback (lambda (button event) (send msg set-label "Button click"))))

(define msg
  (new message%
       (label "No events so far...")
       (parent frame)))



(define my-canvas%
  (class canvas%
    (override on-event on-char)
    (define on-event (lambda (event) (send msg set-label "Canvas mouse")))
    (define on-char (lambda (event) (send msg set-label "Canvas keyboard")))
    (super-instantiate())))

(instantiate my-canvas% (frame))








(send frame show #t)