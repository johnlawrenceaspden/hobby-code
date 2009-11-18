(use 'clojure.contrib.monads)

;;A simple-minded monad comprehension to find pythagorean triples
(defn pyths-sequence-monad-cubic [n]
  (domonad sequence-m
         [a (range 1 n)
          b (range 1 n)
          c (range 1 n) :when (=(+ (* a a) (* b b)) (* c c))]
         (list a b c)))

(pyths-sequence-monad-cubic 20)

;;lots of redundancy in there
(defn pyths-set-monad-cubic [n]
  (domonad set-m
           [a (range 1 n)
            b (range 1 n)
            c (range 1 n) :when (=(+ (* a a) (* b b)) (* c c))]
            (sort (list a b c))))

(pyths-set-monad-cubic 20)

;;some triangles are multiples of each other. Let's ignore them too.
(defn gcd[a b]
  (if (> a b) (gcd b a)
      (if (= a 0) b
          (gcd a (rem b a)))))

(defn base-tri [a b c]
  (let [g (gcd a b)
        h (gcd g c)]
    (sort (map #(/ % h) (list a b c)))))

(defn pyths-set-monad-cubic-unique [n]
  (domonad set-m
           [a (range 1 n)
            b (range 1 n)
            c (range 1 n) :when (=(+ (* a a) (* b b)) (* c c))]
            (base-tri a b c)))

(pyths-set-monad-cubic-unique 20)
;;That's the answer we're looking for
;;but it's order n^3. Let's pre-compute the squares, and also notice that there's
;;no point testing (a b) and (b a) every time.

(defn pyths-set-monad-sqrt-lookup [n]
  (let [ sqrts (apply sorted-map (mapcat #(list (* % %) %) (range 1 n)))]
  (domonad set-m
           [
            a (range 1 n)
            b (range a n)
            c (list (sqrts (+ (* a a) (* b b)))) :when c
            ]
         (base-tri a b c ))))

;;Actually 
(defn pyths-set-monad-sqrt-in-monad [n]
  (domonad set-m
           [
            a (range 1 n)
            b (range a n)
            c (list (Math/sqrt (+ (* a a) (* b b))))
            d (list (Math/round c)) :when (= c d)
            ]
         (base-tri a b d )))

(defn isqrt[n]
     (let [a (Math/sqrt n)
           b (Math/round a)]
       (if (= b a) b nil)))

(map isqrt (range 20))

(defn pyths-set-monad-isqrt [n]
  (domonad set-m
           [
            a (range 1 n)
            b (range a n)
            c (list (isqrt (+ (* a a) (* b b)))) :when c
            ]
         (base-tri a b c )))

(defn pyth [a b]
  (let [c (isqrt (+ (* a a) (* b b)))]
    (if c (base-tri a b c) nil)))

(map (fn[[a b]](pyth a b)) '((3 4)(3 5)(6 8)))

(defn pyths-for-loop [n]
  (clojure.set/difference (set (for [a (range 1 n) b (range a n)] (pyth a b))) (set '(nil))))

(defn pyths-sequence-monad [n]
  (clojure.set/difference 
   (set (domonad sequence-m [a (range 1 n) b (range a n)] (pyth a b)))
   (set '(nil))))

(defn pyths-set-monad [n]
  (clojure.set/difference 
   (set (domonad set-m [a (range 1 n) b (range a n)] (pyth a b)))
   (set '(nil))))

(defn pyths-recursion 
  ([n] 
     (set (pyths-recursion n n '())))
  ([a b acc]
     (if (and (= 1 a) (= 1 b)) 
       acc
       (let [x (pyth a b)
             nacc (if x (cons x acc) acc)]
         (if (= 1 b) (pyths-recursion (dec a) (dec a) nacc)
             (pyths-recursion a (dec b) nacc))))))

(pyths-recursion 20)

(defn pyths-loop
  ([n] 
     (set (pyths-loop n n '())))
  ([a b acc]
     (if (and (= 1 a) (= 1 b)) 
       acc
       (let [x (pyth a b)
             nacc (if x (cons x acc) acc)]
         (if (= 1 b) (recur (dec a) (dec a) nacc)
             (recur a (dec b) nacc))))))

(pyths-loop 20)


;;Timing functions

(defmacro timer [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         finish# (. System (nanoTime))]
     [ret# (double (- finish# start#))]))

(timer (pyths-set-monad 20))

(defn asymptime [f rng]
  (map (fn[n](/ (Math/round (let [[r s] (timer (doall (f n)))]
                (/ s 10 (* n n)))) 100.0))
       rng))

(asymptime pyths-loop '(20 40 80 160 320))

(defn re-filter [re lst]
  (filter (fn [s] (re-matches re (str s))) lst))

(defn partition-set [pred st]
  [(set (filter pred st))
   (set (filter (fn[x] (not (pred x))) st))])

(partition-set #(> 10 %) (set (range 20)))

;;pull all the defined function names out of the namespace
(def pythfns (re-filter #"pyths-.*" (map first (ns-interns 'user))))
(def cubic-pythfns (re-filter #".*cubic.*" pythfns))
(def quadratic-pythfns (clojure.set/difference (set pythfns) cubic-pythfns))

(def broken-pythfns (set '(pyths-recursion)))
(def non-broken-pythfns (clojure.set/difference (set pythfns) broken-pythfns))

(defn sorted-by-timings [fn-symbols-list test-range]
     (sort #(> (last (second %1)) (last (second %2)))
           (map (fn[f] [f
                        (asymptime 
                         (eval f) ;;need eval to turn symbol into function
                         test-range)]) 
                fn-symbols-list)))

(def sorted-timings (sorted-by-timings pythfns (list 10 20 40)))

(let [[a b] (partition-set #(> (last (second %)) 10) sorted-timings )]
  (def slow-pythfns (set (map first a)))
  (def fast-pythfns (set (map first b))))

(def fast-non-broken (clojure.set/difference fast-pythfns broken-pythfns))


(sorted-by-timings '(pyths-recursion pyths-loop) (range 70 80))
(comment
  [pyths-loop      (2.78 2.83 6.96 3.56  4.94 7.81  2.8  1.96 2.71 39.36)] 
  [pyths-recursion (3.77 2.02 2.25 15.28 2.03 3.02 45.61 3.22 6.61  2.02)])

(sorted-by-timings fast-non-broken '(80 160 320 640))
(comment
  [pyths-sequence-monad  (5.92  16.64 11.74 15.93)] 
  [pyths-set-monad-isqrt (13.32 19.93 16.76 12.3)] 
  [pyths-for-loop        ( 4.0   4.02  4.04 4.11)] 
  [pyths-loop            ( 2.4   2.18  2.19 2.34)]))

(sorted-by-timings '(pyths-loop pyths-for-loop) '(80 160 320 640 1280 2560))
(comment
  [pyths-for-loop (2.84 2.81 2.7  2.62 6.82 3.11)] 
  [pyths-loop     (3.79 1.99 1.21 1.18 1.16 1.14)])

(comment
([pyths-set-monad-sqrt-lookup (8.91 9.03 9.04 9.24)] 
 [pyths-set-monad-isqrt       (7.86 7.86 7.52 7.49)] 
 [pyths-sequence-monad        (7.77 5.28 4.83 4.76)] 
 [pyths-for-loop              (3.02 2.55 2.55 2.47)] 
 [pyths-loop                  (1.63 1.25 1.21 1.18)]))