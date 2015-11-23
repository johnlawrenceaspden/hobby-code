#lang web-server/insta
(define (start request)
  (response/xexpr
   '(html
     (head (title "My Blog"))
     (body (h1 "Under construction")))))