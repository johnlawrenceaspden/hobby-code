#lang web-server/insta

(require "model.ss")

;;definitions of web pages

(define (start request)
  (render-blog-page request))

(define (blog-page title body)
    `(html (head (title ,title))
           (link ((rel "stylesheet")
                  (href "/continue_tutorial.css")
                  (type "text/css")))
           (body ,@body)))

(define (render-blog-page request)
  (local [(define (response-generator make-url)
            (blog-page "My Blog" 
                      `((h1 "My Blog" 
                            ,(render-posts BLOG make-url) 
                            (form ((action ,(make-url insert-post-handler)))
                                  (input ((name "title")))
                                  (input ((name "body")))
                                  (input ((type "submit"))))))))
          (define (parse-post bindings)
            (make-post (extract-binding/single 'title bindings)
                       (extract-binding/single 'body  bindings)
                       '()))
          (define (insert-post-handler request)
            (blog-insert-post! 
             BLOG (parse-post (request-bindings request)))
             (render-blog-page (redirect/get)))]
    (send/suspend/dispatch response-generator)))               

(define (render-post-detail-page a-blog a-post request)
  (local [(define (response-generator make-url)
            (blog-page "Post Details" 
                       `((a ((href ,(make-url render-blog-page))) "back to blog")
                         (h1 "Post Details")
                         (h2 ,(post-title a-post))
                         (p ,(post-body a-post))
                         ,(render-as-itemized-list (post-comments a-post))
                         (form ((action
                                 ,(make-url insert-comment-handler)))
                               (input ((name "comment")))
                               (input ((type "submit")))))))
          
          (define (parse-comment bindings)
            (extract-binding/single 'comment bindings))
          
          (define (insert-comment-handler request)
            (render-confirm-add-comment-page
             a-blog
             (parse-comment (request-bindings request))
             a-post
             request))]
    
    (send/suspend/dispatch response-generator)))


(define (render-confirm-add-comment-page blog a-comment a-post request)
  (local [(define (response-generator make-url)
            (blog-page "Add a comment" `(
                    (h1 "Add a comment")
                    "The comment: " (div (p ,a-comment))
                    "will be added to "
                    (div ,(post-title a-post))
                    (p (a ((href ,(make-url yes-handler)))
                          "Yes, add the comment."))
                    (p (a ((href ,(make-url cancel-handler)))
                          "No, I changed my mind")))))
          (define (yes-handler request)
            (post-insert-comment! blog a-post a-comment)
            (render-post-detail-page blog a-post (redirect/get)))
          (define (cancel-handler request)
            (render-post-detail-page blog a-post (redirect/get)))]
    (send/suspend/dispatch response-generator)))


(define (render-post blog a-post make-url)
  (local [(define (view-post-handler request)
            (render-post-detail-page blog a-post request))]
    `(div ((class "post"))
          (a ((href ,(make-url view-post-handler)))
             ,(post-title a-post))
          (p ,(post-body a-post))
          (div ((class "comment-number")) ,(number->string (length (post-comments a-post)))
               "comment (s)"))))
             
(define (render-posts blog make-url)
  (local [(define (render-post/make-url a-post)
            (render-post blog a-post make-url))]
    `(div ((class "posts")) 
          ,@(map render-post/make-url (blog-posts BLOG)))))

(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

(define (render-as-item a-fragment)
  `(li ,a-fragment))

(static-files-path "htdocs")
#|



(define (can-parse-post? bindings)
  (and (exists-binding? 'title bindings)
       (exists-binding? 'body  bindings)))

(define (start request) 
  (local [(define a-blog
            (cond[(can-parse-post? (request-bindings request))
                  (cons (parse-post (request-bindings request)) BLOG)]
                 [else BLOG]))]
  (render-blog-page a-blog request)))

'(p "This is an example")
'(a ((href "link.html")) "Past")
'(p "This is" (div ((class "emph")) "another") "example.")

(define (render-greeting a-name)
  `(html (head (title "Welcome"))
         (body (p ,(string-append "Hello" a-name)))))

(render-post (car BLOG))



|#