(load "johnlib.scm")

(define (scan host ports)
  (map
    (lambda (p) (list p (port->name p)))
    (open-ports host ports)))

(require (lib "list.ss"))

(define (open-ports host ports)
    (filter (lambda (x) (not (eq? 'closed x)))
          (threaded-map
           (lambda (port) (if (can-connect? host port) port 'closed))
           ports)))

(define (can-connect? host port)
  (with-handlers ([exn:i/o:tcp? (lambda (e) #f)])
    (let-values ([(ip op) (tcp-connect host port)])
      (close-input-port ip) (close-output-port op) #t)))

(define (threaded-map f l)
  (let ((cs (map (lambda (x) (make-channel)) l)))
    (for-each (lambda (x c) (thread (lambda () (channel-put c (f x))))) l cs)
    (map channel-get cs)))
(require (lib "url.ss" "net")) ; for get-pure-port and string->url

 (define-syntax (while stx)
  (syntax-case stx ()
    [(_ var test body)
     (identifier? #'var)
     #'(let loop ((var test))
         (when var body (loop test)))]))
 
(define NAMES
  (let ([ip (if (file-exists? "/etc/services")
                (open-input-file "/etc/services")
                (get-pure-port (string->url "http://www.iana.org/assignments/port-numbers")))]
        [nametable (make-hash-table)])
    (while m (regexp-match "([^ \t\r\n]+)[ \t]+([0-9]+)/tcp[ \t]+([^\r\n])" ip)
       (hash-table-put! nametable (string->number (list-ref m 2)) (list-ref m 1)))
    nametable))

 

(define (port->name p) (hash-table-get NAMES p (lambda () "unknown")))
(scan "192.168.0.2" (enumerate-interval 1 1000))