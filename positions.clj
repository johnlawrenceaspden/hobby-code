;;; http://stackoverflow.com/questions/1726943/is-functional-clojure-or-imperative-groovy-more-readable


(defn redux [[current next] flag] [(if flag current next) (inc next)])

(defn positions [coll]
  (map first (reductions redux [1 2] (map = coll (rest coll)))))

(def scores '( 40 40 39 38 37 37 35 32 32 ))

(reductions redux [1 2] (map = '(1 1 ) (rest '(1 1))))

(use 'clojure.test)

(defn test-positions [p] 
  (and
   (is (= (p '()) '()))
   (is (= (p '(1)) '(1)))
   (is (= (p '(1 1)) '(1 1)))
   (is (= (p '(1 2)) '(1 2)))
   (is (= (p '(1 1 2)) '(1 1 3)))
   (is (= (p '(1 1 2 2 3)) '(1 1 3 3 5)))
   (is (= (p '( 40 40 39 38 37 37 35 32 32 )) '(1 1 3 4 5 5 7 8 8)))))

(test-positions positions)



;; 

(defn positions  
  ([s] (positions s 1 1 (first s)))
  ([s  pos cur-pos cur-score]
                (when-not (empty? s)
                  (if ( = (first s) cur-score)
                    (cons cur-pos (positions (rest s) (inc pos) cur-pos cur-score))
                    (cons pos     (positions (rest s) (inc pos) pos     (first s)))))))

(try-position positions)



(defn positions  
  ([s] (positions s 1 1 (first s)))
  ([s  pos cur-pos cur-score]
     (lazy-seq
      (if (empty? s) '()
          (if ( = (first s) cur-score)
            (cons cur-pos (positions (rest s) (inc pos) cur-pos cur-score))
            (cons pos     (positions (rest s) (inc pos) pos     (first s))))))))



(defn positions  
  ([s] (positions s 1 1 (first s)))
  ([s  pos cur-pos cur-score]
     (lazy-seq
      (when-not (empty? s)
        (if ( = (first s) cur-score)
          (cons cur-pos (positions (rest s) (inc pos) cur-pos cur-score))
          (cons pos     (positions (rest s) (inc pos) pos     (first s))))))))


(defn positions  
  ([s] (positions s 1 1 (first s)))
  ([s  pos cur-pos cur-score]
                (when-not (empty? s)
                  (if ( = (first s) cur-score)
                    (cons cur-pos (positions (rest s) (inc pos) cur-pos cur-score))
                    (cons pos     (positions (rest s) (inc pos) pos     (first s)))))))



(positions (list 40 40 39 38 37 37 35 32 32 ))
(positions (list 1 1 2))
(positions (list 1 2 3 4 4))








(defn positions [coll]
  (loop [coll coll
         current 1
         next 1
         previous (first coll)
         positions []]
    (if (= previous (first coll))
      (conj positions current)
      (recur (rest coll) 
