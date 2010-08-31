(defn factors [n]
  (loop [n n ,test 2, acc '(), sofar 1]
    (when (integer? (/ test 10000)) 
      (println n test acc sofar))
    (if (= n 1) 
      (reverse acc)
      (if (< n (* test test))
        (reverse (cons n acc))
        (if-not (integer? (/ n test))
          (recur n (inc test) acc sofar)
          (recur (/ n test) test (cons test acc) (* sofar test)))))))
      
(factors 1000)
;;(factors 9165379787907897767841097481074801)
(factors 9165379787902562387)

(* 2 13 7523)


(def fibs
     (lazy-seq
       (cons 0
             (cons 1
                   (map + fibs (drop 1 fibs))))))

(take 10 fibs)

(def integers (iterate inc 2))

(defn sieve [seq]
     (let [p (first seq)]
       (cons p 
             (lazy-seq
               (filter #(not (= (mod % p) 0)) (sieve (next seq)))))))

(def primes (sieve integers))

(take 30 primes)


(defn factors [n]
  (loop [n n , tests primes, acc '()]
    (let [p (first tests) r (next tests)]
      (cond (= n 1)          (reverse acc)
            (< n (* p p))    (reverse (cons n acc))
            (= 0 (mod n p))  (recur (/ n p) tests (cons p acc) )
            :else            (recur n r acc)))))

(doall (map factors (range 110)))
(factors 2)
(factors 3)
(factors 4)
(factors 5)


(defn infinite-sieve 
  ([n] (sort (map second (infinite-sieve (sorted-set [2,2]) 2 n))))
  ([testset int stop]
  (if (> int stop) testset
      (let [pair (first testset)
            [multiple prime] pair]
        (cond (= int multiple) (recur testset (inc int) stop)
              (> int multiple) (recur (conj (disj testset pair) [(+ multiple prime) prime]) int stop)
              (< int multiple) (recur (conj testset [(* int int) int]) (inc int) stop))))))


(defn sieve-iteration [[output multiple-set sequence]]
  (let [[m p :as f] (first multiple-set)
        c           (first sequence)]
    (cond (= m c) (list nil multiple-set (rest sequence))
          (> m c) (let [new-set (conj multiple-set [(* c c) c])]
                    (list c new-set (rest sequence)))
          (< m c) (let [new-set (conj (disj multiple-set f) [(+ m p) p])]
                    (list nil new-set sequence)))))

(def primes 
     (filter #(not (nil? %)) 
             (map first  
                  (iterate sieve-iteration 
                           [2 (sorted-set [2 2]) (iterate inc 2)]))))

(take 100 primes)

(take 100 (map first  
               (iterate sieve-iteration 
                        [2 (sorted-set [2 2]) (iterate inc 2)])))

(take 3 (iterate sieve-iteration 
                   [2 (sorted-set [2 2]) (take 20 (iterate inc 2))]))


(sieve-iteration
       (sieve-iteration [2 (sorted-set [2 2]) '( 2 3 4 5 6 7 8 )]))

(take 100 (filter #(not (nil? %)) (map first  (iterate sieve-iteration (list 2 (sorted-set [2 2]) (iterate inc 2))))))


