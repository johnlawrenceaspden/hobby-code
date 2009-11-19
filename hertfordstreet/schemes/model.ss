#lang scheme
;;definitions of posts and blogs

(define-struct blog (home posts) #:mutable #:prefab)

(define-struct post (title body comments) #:mutable #:prefab)



#|(define BLOG 
  (make-blog "blogfile"
   (list
    (make-post
     "Got the double submit problem fixed"
     "By the mysterious use of get/request"
     '("ah yes, but can you store and restore the state of the thing?"
       "It would seem so!"
       "even after splitting the model.ss file out?"))
    (make-post
     "I think I'll add a post"
     "Because I am feeling quite happy"
     '(""
       "this css business seems to work pretty well!"
       "smuggity smuggity smug smug"
       "now to add color to all pages"))
    (make-post "Fourth Post" "Added through web page" '())
    (make-post
     "Third Post!"
     "Hey, this is my third post"
     '("comment 3a" "comment 3b"))
    (make-post
     "Second Post!"
     "Hey, this is my second post"
     '("comment 2"
       "program added comment here"
       "and a thired comment added through the web page"
       "and a thired comment added through the web page"))
    (make-post
     "First Post!"
     "Hey, this is my first post"
     '("comment 1" "web page comment")))))|#

(define (initialize-blog! home)
  (local [(define (log-missing-exn-handler exn)
            (make-blog
             (path->string home)
             (list (make-post "first post" "this is my first post" '("first comment")))))
          (define the-blog
            (with-handlers ((exn? log-missing-exn-handler))
              (with-input-from-file home read)))]
    (set-blog-home! the-blog (path->string home))
    the-blog))

(define BLOG (initialize-blog! (string->path "blogfile")))

(define (save-blog! a-blog)
  (local [(define (write-to-blog)
            (write a-blog))]
    (with-output-to-file (blog-home a-blog)
      write-to-blog
      #:exists 'replace)))

(define (blog-insert-post! blog post)
  (set-blog-posts! blog (cons post (blog-posts blog)))
  (print "saving now")
  (save-blog! blog)
  (print "post-save"))

(define (post-insert-comment! blog post comment)
  (set-post-comments! post (append (post-comments post) (list comment)))
  (save-blog! blog))

(define (blog->code BLOG)
  `(define BLOG (make-blog (list 
                            ,@(map (λ(p) `( make-post ,(post-title p) ,(post-body p) (quote ,(post-comments p)))) 
   (blog-posts BLOG))))))

(provide (all-defined-out))