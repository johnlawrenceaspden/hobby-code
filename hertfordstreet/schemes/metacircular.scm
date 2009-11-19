(define eval
  (lambda (exp env)
    (cond ((number? exp) exp)
          ((symbol? exp) (lookup exp env))
          ((eq? (car exp) 'quote) (cadr exp))
          ((eq? (car exp) 'lambda) (list 'closure (cdr exp) env))
          ((eq? (car exp) 'cond)
           (evcond (cdr exp) env))
          (else (apply (eval (car exp) env) (evlist (cdr exp) env))))))


(define apply
  (lambda (proc args)
    (cond ((primitive? proc)
           (apply-primop proc args))
          ((eq? (car proc) 'closure)
           (eval (cadadr proc)
                 (bind (caadr proc)
                       args
                       (caddr proc))))
          (else error))))


(define evlist
  (lambda (l env)
    (cond ((eq? l '()) '())
          (else (cons (eval (car l) env) (evlist (cdr l) env))))))

(define evcond
  (lambda (clauses env)
    (cond ((eq? clauses '()) 'error)
          ((eq? (caar clauses) 'else) (eval (cadar clauses) env))
          ((eval (caar clauses) env) (eval (cadar clauses) env))
          (else (evcond (cdr clauses) env)))))

(define bind
  (


evcond
lookup

primitive?
apply-primop
bind
           

          
          
      