(require (lib "class.ss"))

(define person%
  (class object%
    (init-field name)
    (field ( friends '()))
    (super-new)
    
    (define/public (add-friend other)
      (set! friends (cons other friends)))
    
    (define/public (meet-all)
      (for-each
       (lambda (f) (printf "hi ~a~%" (get-field name f)))
       friends))
    
    (define/public (say-hello)
      (printf "hello, my name is ~a~%" name))))

(define valley-person%
  (class person%
    (inherit-field name)
    (super-new)
    (define/public (do-lunch other)
      (send this add-friend other)
      (send other add-friend this))
    (define/override (say-hello)
      (printf "like, so totally, hello. I'm ~a. Duh!~%" name))))

(define-syntax dude
  (syntax-rules ()
    ((dude a)
     (define a (new person% (name (format "~a" (quote a))))))))

(define-syntax make-friends
  (syntax-rules ()
    ((make-friends a b)
     (begin 
       (send a add-friend b)
       (send b add-friend a)))))

;the program

(dude buffy)
(dude xander)
(dude willow)
(define cordelia (new valley-person% (name "cordelia")))

(make-friends xander willow)
(make-friends buffy willow)
(make-friends buffy xander)
(send xander add-friend cordelia)

(map (lambda (x) (send x meet-all)) (list  xander buffy willow cordelia))

(send cordelia say-hello)

(get-field name buffy)
(get-field friends buffy)







