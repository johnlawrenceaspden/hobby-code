;; I often find myself wanting to iterate something until it converges.

(defn collatz [n] (if (even? n) (recur (/ n 2)) (+ 1 (* 3 n))))

(iterate collatz 100) ; (100 76 58 88 34 52 40 16 4 4 4 4 4 4 ...)

;; And so I often find myself writing:

(defn take-while-unstable
  ([sq] (if (empty? sq) '()
            (cons (first sq) (#'take-while-unstable (rest sq) (first sq)))))
  ([sq last] (cond (empty? sq) '()
                   (= (first sq) last) '()
                   :else (cons (first sq) (#'take-while-unstable (rest sq) (first sq))))))

;; Which allows me to stop the iteration once it's settled down.
(take-while-unstable (iterate collatz 100)) ; (100 76 58 88 34 52 40 16 4)

;; You can see how it works here:
(binding  [*print-length* 20] ; taking care not to look at the medusa
  (cct/dotrace (take-while-unstable) (take-while-unstable (iterate collatz 100))))

;; TRACE t15884: (take-while-unstable (100 76 58 88 34 52 40 16 4 4 4 4 4 4 4 4 4 4 4 4 ...))
;; TRACE t15885: |    (take-while-unstable (76 58 88 34 52 40 16 4 4 4 4 4 4 4 4 4 4 4 4 4 ...) 100)
;; TRACE t15886: |    |    (take-while-unstable (58 88 34 52 40 16 4 4 4 4 4 4 4 4 4 4 4 4 4 4 ...) 76)
;; TRACE t15887: |    |    |    (take-while-unstable (88 34 52 40 16 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 ...) 58)
;; TRACE t15888: |    |    |    |    (take-while-unstable (34 52 40 16 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 ...) 88)
;; TRACE t15889: |    |    |    |    |    (take-while-unstable (52 40 16 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 ...) 34)
;; TRACE t15890: |    |    |    |    |    |    (take-while-unstable (40 16 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 ...) 52)
;; TRACE t15891: |    |    |    |    |    |    |    (take-while-unstable (16 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 ...) 40)
;; TRACE t15892: |    |    |    |    |    |    |    |    (take-while-unstable (4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 ...) 16)
;; TRACE t15893: |    |    |    |    |    |    |    |    |    (take-while-unstable (4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 ...) 4)
;; TRACE t15893: |    |    |    |    |    |    |    |    |    => ()
;; TRACE t15892: |    |    |    |    |    |    |    |    => (4)
;; TRACE t15891: |    |    |    |    |    |    |    => (16 4)
;; TRACE t15890: |    |    |    |    |    |    => (40 16 4)
;; TRACE t15889: |    |    |    |    |    => (52 40 16 4)
;; TRACE t15888: |    |    |    |    => (34 52 40 16 4)
;; TRACE t15887: |    |    |    => (88 34 52 40 16 4)
;; TRACE t15886: |    |    => (58 88 34 52 40 16 4)
;; TRACE t15885: |    => (76 58 88 34 52 40 16 4)
;; TRACE t15884: => (100 76 58 88 34 52 40 16 4)
;; (100 76 58 88 34 52 40 16 4)

;; Anyway, this function comes in so handy that I thought I'd lazyize it and give it some tests

(use 'clojure.test)

(with-test
  
  (defn take-while-unstable
    ([sq] (lazy-seq (if-let [sq (seq sq)]
                      (cons (first sq) (take-while-unstable (rest sq) (first sq))))))
    ([sq last] (lazy-seq (if-let [sq (seq sq)]
                           (if (= (first sq) last) '() (take-while-unstable sq))))))

  (is (= (take-while-unstable '()) '()))
  (is (= (take-while-unstable '(1)) '(1)))
  (is (= (take-while-unstable '(1 1)) '(1)))
  (is (= (take-while-unstable '(1 2)) '(1 2)))
  (is (= (take-while-unstable '(1 1 2)) '(1)))
  (is (= (take-while-unstable '(1 1 1)) '(1)))
  (is (= (take-while-unstable '(1 1 2 1)) '(1)))
  (is (= (take-while-unstable '(1 2 3 4 5 6 7 7 7 7)) '(1 2 3 4 5 6 7)))
  (is (= (take 10000 (take-while-unstable (range))) (take 10000 (range))))
  (is (= (take-while-unstable (concat (take 10000 (range)) '(10000) (drop 10000 (range)))) (range 10001)))
  (is (= (take-while-unstable '(nil)) '(nil)))
  (is (= (take-while-unstable '(nil nil)) '(nil)))
  (is (= (take-while-unstable '[nil nil]) '(nil)))
  (is (= (take-while-unstable [ :a :b :c :d :d :e]) '(:a :b :c :d))))

(run-tests) ; {:type :summary, :test 1, :pass 14, :fail 0, :error 0}

;; Hopefully someone will find it useful. Can anyone break it?