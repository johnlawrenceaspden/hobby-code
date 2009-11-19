;http://download.plt-scheme.org/doc/206p1/html/mzscheme/mzscheme-Z-H-12.html#node_chap_12
;had to modify it to run even under pretty big
;change rename to rename-out and quote the module names in the require and module statements

(module lambda-calculus scheme

  (define-syntax lc-lambda
    (syntax-rules ()
      ((_ (x) E) (lambda (x) E))))
  
  (define-syntax lc-app
    (syntax-rules ()
      ((_ E1 E2) (E1 E2))))
  
  (define-syntax lc-module-begin
    (syntax-rules ()
      ((_ E) (#%module-begin E))))
  
  (define-syntax lc-datum
    (syntax-rules ()))
  
  (provide #%top
           (rename-out (lc-lambda lambda)
                       (lc-app #%app)
                       (lc-module-begin #%module-begin)
                       (lc-datum #%datum))))

(module m 'lambda-calculus
  ((lambda (y) (y y))
   (lambda (y) (y y))))

(require 'm)