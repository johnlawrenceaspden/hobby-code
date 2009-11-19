#lang scheme/gui

(define (ask-yes-or-no-question #:question question
                                #:default answer
                                #:title (title "Yes or No?")
                                #:width (w 400)
                                #:height (h 200))
  
  (define d (new dialog% [label title] [width w] [height h]))
  (define msg (new message% [label question] [parent d]))
  (define (yes) (set! answer #t) (send d show #f))
  (define (no) (set! answer #f) (send d show #f))
  (define yes-b (new button%
                     [label "Yes"] [parent d]
                     [callback (λ (x y) (yes))]
                     [style (if answer '(border) '())]))
  
  (define no-b (new button%
                    [label "No"] [parent d]
                    [callback (λ (x y) (no))]
                    [style (if answer '() '(border))]))
  
  (send d show #t)
  answer)



(provide/contract
 [ask-yes-or-no-question
  (->* (#:question string?
                   #:default boolean?)
       (#:title string?
                #:width exact-integer?
                #:height exact-integer?)
      boolean?)])