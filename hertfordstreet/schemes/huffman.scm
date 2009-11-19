(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))









  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))


(define (decode bits tree)

  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (encode-symbol s tree)
  (cond ((leaf? tree) '())
        ((memq s (symbols (left-branch tree))) (cons '0 (encode-symbol s (left-branch tree))))
        ((memq s (symbols (right-branch tree))) (cons '1 (encode-symbol s (right-branch tree))))
        (else (error "no such symbol in tree"))))


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))



(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define (merge pairs)
    (adjoin-set (make-code-tree (car pairs) (cadr pairs)) (cddr pairs)))
                                         
(define (successive-merge pairs)
  (if (null? (cdr pairs)) (car pairs)
      (successive-merge (merge pairs))))

(define 50s '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))

(encode '(GET A JOB SHA NA NA NA NA NA NA 
              NA NA GET A JOB SHA NA NA NA NA NA NA NA NA 
              WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM) (generate-huffman-tree 50s))




;(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;
;(define sample-message-symbols (decode sample-message sample-tree))
;
;(equal? (encode sample-message-symbols sample-tree) sample-message)

;(define sample-freqs (make-leaf-set '((A 4) (B 2) (C 1) (D 1))))






