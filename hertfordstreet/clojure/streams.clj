(def cyc123 (cycle [1 2 3]))

(every? (fn [x] (= 1 x)) cyc123)

(take 5 cyc123)

(take 5 (filter (fn [x] (= x 1)) cyc123))

(take 5 (drop 1 cyc123))

(take 5 (interleave cyc123 cyc123))

(take 5 (take-nth 2 cyc123))

;doesn't work.. stupid blog
;(take 10 (for [x cyc123] (< x 3) x))

(take 10 (filter #(< % 3) cyc123))

(mapcat (fn [x y] (list x y)) cyc123 '(10 11 12))

(defn sieve [s]
  (lazy-cons (first s)
	     (sieve (filter
		     (fn [x]
			 (not 
			  (= (rem x (first s)) 0)))
		     (rest s)))))

(def primes (sieve (iterate inc 2)))

(time (reduce + (take 1000 primes)))