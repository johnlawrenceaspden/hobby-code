;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Anyway this very evening Hermann Hauser of this parish is telling
;; me that Machine Learning is the coming next big thing and those
;; skills are highly in demand.

;; So if anybody wants to give me a fuckload of money for this old
;; rope, they must feel free

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Naive Bayesian Classifier

;; Ed Jackson ( http://boss-level.com ) and I are currently working
;; our way through Kevin Murphy's book:
;; Machine Learning: A Probabilistic Perspective.

;; Ed tells me that there is big money in this Machine Learning game.

;; We have just read chapter 3, which involves a hair-raising amount
;; of Greek, but when all is said and done boils down to this:

;; We got Martians, who tend to be thin, tall and green
(defn make-martian []
  #{:martian
    (if (< (rand) 0.2) :fat :thin)
    (if (< (rand) 0.7) :tall :short)
    (if (< (rand) 0.8) :green :blue)})

; And we got Venusians, who tend to be fat and blue
(defn make-venusian []
  #{:venusian
    (if (< (rand) 0.8) :fat :thin)
    (if (< (rand) 0.5) :tall :short)
    (if (< (rand) 0.3) :green :blue)})

; They visit Harry's 
(defn make-space-bar-patron []
  (if (< (rand) 0.3) (make-martian) (make-venusian)))

; Harry takes notes
(def client-database
  (for [i (range 10000)] (make-space-bar-patron)))

; Like this:
(take 5 client-database)
(#{:tall :martian :blue :thin} 
 #{:blue :venusian :short :fat} 
 #{:blue :venusian :short :thin} 
 #{:tall :martian :green :fat} 
 #{:martian :blue :short :thin})

; And tabulates them so:
(def martians (filter :martian client-database))
(def venusians (filter :venusian client-database))

(count martians) ; 3054
(count (filter :thin martians)) ; 2454
(count (filter :green martians)) ; 2438
(count (filter :tall martians)) ; 2177


(count venusians) ; 6946
(count (filter :fat venusians)) ; 5628
(count (filter :blue venusians)) ; 4919
(count (filter :tall venusians)) ; 3535


; A guy walks into Harry's
(def new-guy (make-space-bar-patron))

; But he is not wearing his uniform
(reduce disj new-guy [:martian :venusian])  ; #{:blue :short :fat}

; And so a sweepstake comes into being:
(defn probability [characteristic given-class prior-for prior-against]
  (let [class (if given-class (filter given-class client-database) client-database)
        for (count (filter characteristic class))
        against (- (count class) for)
        total-for (+ prior-for for)
        total-against (+ prior-against against)]
    (/ total-for (+ total-for total-against))))



(def m (* (probability :martian nil 1 1)      ; most people come in here are venusians
          (probability :blue  :martian 1 1)   ; and martians tend to be green
          (probability :short :martian 1 1)   ; also he's short. Martians are tall, often
          (probability :fat   :martian 1 1))) ; and fat. All the fat guys is from Venusburg

(def v (* 
        (probability :venusian nil 1 1)       ; most likely he's from venus
        (probability :blue  :venusian 1 1)    ; the blueness bears this out
        (probability :short :venusian 1 1)    ; sometimes they are short, sometimes they are tall
        (probability :fat   :venusian 1 1)))  ; but they are mostly fat

(float (/ m (+ m v))) ; 0.017495114
(float (/ v (+ m v))) ; 0.9825049

;; 45:1 he's from Venus !

new-guy ; #{:blue :short :venusian :fat}

;; As indeed he is.








