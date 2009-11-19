(require (lib "class.ss"))
(require (lib "list.ss"))

(define list%
  (class object%
    (init-field data)
    (super-new)
    (define/public (fold f acc)
      (foldl f acc data))))

(define vector%
  (class object%
    (init-field data)
    (super-new)
    (define/public (fold f acc)
      (let ((N (vector-length data)))
        (let loop ((i 0)
                   (acc acc))
          (cond
            ((= i N) acc)
            (else (loop (add1 i) (f (vector-ref data i) acc)))))))))

(define (foreach-mixin class%)
  (class class%
    (super-new)
    (inherit fold)
    (define/public (for-each f)
      (send this fold (lambda (x _) (f x)) (void)))))

(define list2% (foreach-mixin list%))
(define vector2% (foreach-mixin vector%))
            

(define my-list (new list2% (data '(3 1 4 1 5 9 2 6))))
(define my-vec (new vector2% (data #(3 1 4 1 5 9 2 6))))


(send my-vec for-each (lambda(x) (printf "~a~n" x)))

(load "classes.scm")

(define valley-person2% (foreach-mixin valley-person%))

(define leia (new valley-person2% (name "leia organa")))

(send leia say-hello)

(send leia for-each (lambda(x) (printf "~a~n" x)))