;single step macro expansion
(define (check-macro input)
  (syntax-object->datum (expand-once input)))

(define (thingy p)
  (list p (port->name p)))

(define (scan host ports)
  (map thingy 
       (open-ports host ports)))

(define (range low high)
  (cond
    [(> low high) null]
    [else (cons low (range (+ low 1) high))]))

(require (lib "list.ss"))

(define (open-ports host ports)
  (filter (lambda (x) (not (eq? 'closed x)))
          (threaded-map
           (lambda (port) (if (can-connect? host port) port 'closed))
           ports)))

(define (can-connect? host port)
  (with-handlers ([exn:fail:network? (lambda (e) #f)])
    (let-values ([(ip op) (tcp-connect host port)])
      (close-input-port ip) (close-output-port op) #t)))
                     

(define (threaded-map f l)
  (let ((cs (map (lambda (x) (make-channel)) l)))
    (for-each (lambda (x c) (thread (lambda () (channel-put c (f x))))) l cs)
    (map channel-get cs)))

(define-syntax (while stx)
  (syntax-case stx ()
    [(_ var test body)
     (identifier? #'var)
     #'(let loop ((var test))
         (when var body (loop test)))]))

(require (lib "url.ss" "net"))

(define-syntax dbpr
  (syntax-rules ()
    ((dbpr a) 
     (let ((temp a))
       (display 'a) (display  " = ") (display temp) (display "\n") 
       temp))
    ((dbpr a b ...)
     (let ((temp a)) (display 'a) (display " = ") (display temp) (display ", ") (dbpr b ...))
     )))

(define NAMES
  (let ([ip (if (file-exists? "/etc/services")
                (open-input-file "/etc/services")
                (get-pure-port (string->url "http://www.iana.org/assignments/port-numbers")))]
        [nametable (make-hash-table)])
    (while m (regexp-match "([^ \t\r\n]+)[ \t]+([0-9]+)/tcp[ \t]+([^\r\n])" ip)
           (hash-table-put! nametable (string->number (bytes->string/locale (list-ref m 2)))  (bytes->string/locale (list-ref m 1))))
    nametable))


(define (port->name p) (hash-table-get NAMES p (lambda () "unknown")))




