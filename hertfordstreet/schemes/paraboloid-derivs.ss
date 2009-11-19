(module paraboloid-derivs (planet "deriv-lang.ss" ("wmfarr" "deriv.plt" 2))
  (provide paraboloid dpdx dpdy)

  ;; Paraboloid centered on 0.
  (define (paraboloid x y)
    (+ (* 10 x x)
       (* 20 y y)
       30))

  ;; And its partial derivatives.
  (define dpdx ((partial 0) paraboloid))
  (define dpdy ((partial 1) paraboloid)))
