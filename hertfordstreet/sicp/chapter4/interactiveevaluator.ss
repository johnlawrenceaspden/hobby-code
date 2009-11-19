#lang scheme

(define (go) (eval '(begin
                      (require scheme/enter) 
                      (require (lib "trace.ss"))
                      (compile-enforce-module-constants #f)
                      (load "bookevaluator.ss")
                      (enter! "bookevaluator.ss"))))



(printf "
In the interactions window of drscheme, type
> (go)

the meta-evaluator in \"bookevaluator.ss\" should load, without
enforcing the restriction that its procedures all be constant

This allows us to trace execution, eg:

> (me ((λ(a b)(* a b)) 1 2))
2

> (trace apply-primitive-procedure)
> (me ((λ(a b)(* a b)) 1 2))
|(apply-primitive-procedure (primitive #<procedure:*>) (1 2))
|2
2
> ")