;; Diversity Prediction Theorem

;; I've recently seen in various places the Diversity Predition Theorem, variously expressed as:

;; Crowd Error = Average Error - Diversity
;; Being different is as important as being good
;; The Crowd Beats the Average

;; I'm not going to argue with a mathematical tautology, but I'm a bit sceptical about the 
;; happy feelings induced by this law.

;; Consider:

(defn diversity-theorem [truth predictions]
  (let [square (fn[x] (* x x))
        mean (/ (reduce + predictions) (count predictions))
        avg-sq-diff (fn[a] (/ (reduce + (for [x predictions] (square (- x a)))) (count predictions)))]
    {:average-error (avg-sq-diff truth)
     :crowd-error (square (- truth mean))
     :diversity (avg-sq-diff mean)}))

;; We have a thing whose real value is 49, and some experts who guess the value fairly accurately
;; Their average guess is really close!
(diversity-theorem 49 '(48 47 51))    ;-> {:average-error 3, :crowd-error 1/9, :diversity 26/9}

;; We make our population of experts more diverse:
(diversity-theorem 49 '(48 47 51 42)) ;-> {:average-error 29/2, :crowd-error 4, :diversity 21/2}

