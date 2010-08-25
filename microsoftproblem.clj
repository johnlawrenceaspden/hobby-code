;; The X and Y Dialogue

;; Microsoft Research 15 Year Anniversary Riddle:

;; Two integers x and y are selected such that 1 < x <= y < 100.

;; Player A is given the product xy and player B is given the sum x+y.

;; The following dialogue takes place:

;;     * A: I don’t know the values x and y.
;;     * B: I knew you didn’t know. I don’t know them either.
;;     * A: Now I know x and y.
;;     * B: Now I know them too.

;; Can you figure out x and y from this information?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What are all the possible x and y?

(defn xylist [n]
  (for [x (range 2 n) y (range x n)] [x y]))

;; For all the possible x and y, what are all the possible sums and products?

(def possible-products-and-sums (map (fn [[x y]] [(list x y) (* x y) (+ x y)]) (xylist 100)))

(take 10 (sort-by str possible-products-and-sums)) ;; ([(10 10) 100 20] [(10 11) 110 21] ...

(count possible-products-and-sums) ;;4851            Sanity check: 1+2+....+98 = 98*99/2 = 4851

;; Let's collect together all the x,y which make a particular product:
(def sums-indexed-by-product 
     (reduce 
      (fn [map [[x y] product sum]] 
        (assoc map product (cons sum (get map product '())))) 
      {} 
      possible-products-and-sums))

(take 10 sums-indexed-by-product)     ;;([1024 (64 80)] [2048 (96)] [3072 (112 128)] ....
;; 1024 is 2*512, 4*256, 8*128, 16*64, or 32*32, of which only 16*64 and 32*32 are legal, summing to 80 and 64. If we know the product is 1024 then we have two choices for x and y.
;; 2048 is 2*1024, 4*512, 8*256, 16*128, or 32*64, of which only 32*64 is legal. So if we know the product is 2048 then we know x and y.

(count sums-indexed-by-product) ;;2843. There are 2843 distinct possibilities for the product

;; Similarly, let's take all the x,y which produce a particular sum
(def products-indexed-by-sum 
     (reduce 
      (fn [map [[x y] product sum]] 
        (assoc map sum (cons product (get map sum '())))) 
      {} 
      possible-products-and-sums))

(first products-indexed-by-sum) ;; [32 (256 255 252 247 240 231 220 207 192 175 156 135 112 87 60)]
;; sum 32 could be 2+30, 3+29, 4+28 ..., whose products are 60, 87, 112 ...

(count products-indexed-by-sum) ;; 195 There are 195 possible sums, from 2+2 to 99+99

;; A knows the product, but that doesn't give him a unique x,y, hence a unique sum. Therefore A's possibilities are all the products which are associated with more than one sum:

(def A-possibles 
     (filter 
      (fn[[product lst]] 
        (< 1 (count lst))) 
      sums-indexed-by-product))

(count A-possibles)   ;; 1068 cut down from 2843
(take 10 A-possibles) ;; ([1024 (64 80)] [3072 (112 128)] [32 (12 18)] [1056 (65 68 70 82 100 107)] ......

;; Just the possible products, without their associated possible sums
(def A-possible-set (set (map first A-possibles)))

(count A-possible-set)   ;;1068
(take 10 A-possible-set) ;;(1024 3072 32 1056 2080 4128 7200 64 1088 2112)
                        

;; B already knew that A had one of these numbers. Therefore his sum must be one that is only
;; produced by x,y that produce products in A's possible set. 

;; Otherwise he wouldn't *know* that A didn't know.

(def B-possibles 
     (filter 
      (fn[[sum product-list]] 
        (every? A-possible-set product-list)) 
      products-indexed-by-sum))

(first B-possibles) ;; [35 (306 304 300 294 286 276 264 250 234 216 196 174 150 124 96 66)]
;; 35 is a possible sum that B could have been told because every associated product would leave A unsure.
;; 306 could be 17*18 or 6*51 or 9*34, giving sums 35, 57, or 43
(sums-indexed-by-product 306) ;; (35 43 57)


;; Let's make a set out of the sums that would have allowed B to say that he knew that A didn't know:
(def B-possible-set (apply sorted-set (map first B-possibles)))

(count B-possible-set) ;; 10 cut down from 199.

B-possible-set ;;#{11 17 23 27 29 35 37 41 47 53}

;; So the fact that A can't tell from the product somewhat restricts the possible products, and the fact that B knew that A had one of those products restricts the possible sums.

;; Now, we can restrict our considerations to only those x,y which can produce numbers in these restricted sets.
(def new-possible-products-and-sums (filter (fn[[[x y] product sum]] (and (A-possible-set product)(B-possible-set sum))) possible-products-and-sums))

(count new-possible-products-and-sums) ;;145

;; Our possible x,y have gone from 4851 to 145 pairs.

(take 10 new-possible-products-and-sums) ;;([(2 9) 18 11] [(2 15) 30 17] [(2 21) 42 23] ....

;; Let's recalculate what we can say about the sum given the product
(def new-products-indexed-by-sum 
     (reduce 
      (fn [map [[x y] product sum]] 
        (assoc map sum (cons product (get map sum '())))) 
      {} 
      new-possible-products-and-sums))

;; and what we can say about the product given the sum
(def new-sums-indexed-by-product 
     (reduce (fn [map [[x y] product sum]] 
               (assoc map product (cons sum (get map product '())))) 
             {} 
             new-possible-products-and-sums))

(first new-products-indexed-by-sum) ;;[35 (306 304 300 294 286 276 264 250 234 216 196 174 150 124 96 66)]
(first new-sums-indexed-by-product) ;; [96 (35)]

(count new-products-indexed-by-sum) ;; 24
(count new-sums-indexed-by-product) ;; 193

(def possible-products (set (map first new-sums-indexed-by-product)))
(def possible-sums     (set (map first new-products-indexed-by-sum)))

;; Both B and A now know that these are the only possibilities

(map (fn[[sum plist]] (count plist)) new-products-indexed-by-sum)
;; It seems that whatever B knows about the sum isn't enough to tell him the product, because there's always more than one alternative
;; So when he says I don't know either, that seems to be redundant information. Oops.

(map (fn[[prod slist]] (count slist)) new-sums-indexed-by-product)
;; Although most of the time someone who knows the product can now tell you the sum.

;; A, knowing the product, now knows the sum as well. That means he must have one of the products which uniquely determines the sum
(filter (fn[[prod slist]] (= 1 (count slist))) new-sums-indexed-by-product)

(def new-A-possible-set (apply sorted-set (map first (filter (fn[[prod slist]] (= 1 (count slist))) new-sums-indexed-by-product))))

new-A-possible-set ;;#{18 24 28 50 52 54 76 92 96 100 110 112 114 124 130 138 140 148 152 154 160 162 168 170 172 174 176 182 186 190 198 204 208 216 232 234 238 240 246 250 252 270 276 280 282 288 294 304 306 310 336 340 348 360 364 370 378 390 400 408 414 418 430 442 480 492 496 510 520 522 532 540 550 552 570 592 612 630 646 660 672 682 690 696 700 702}

(count new-A-possible-set) ;; 86

(def final-possible-products-and-sums (filter (fn[[[x y] product sum]] (and (new-A-possible-set product)(B-possible-set sum))) possible-products-and-sums))

(count final-possible-products-and-sums)  ;; 86


(def final-sums-indexed-by-product (reduce (fn [map [[x y] product sum]] (assoc map product (cons sum (get map product '())))) {} final-possible-products-and-sums))
(def final-products-indexed-by-sum (reduce (fn [map [[x y] product sum]] (assoc map sum (cons product (get map sum '())))) {} final-possible-products-and-sums))

(count final-sums-indexed-by-product) ;;86
(count final-products-indexed-by-sum) ;;10

(map (fn[[sum plist]] (count plist)) final-products-indexed-by-sum)

(def possible-sums (set (map first (filter (fn[[sum plist]] (= 1 (count plist))) final-products-indexed-by-sum))))

possible-sums ;; #{17} Aha!
;; The sum must have been 17 in order for B to work it out.

;; The product must have been in new-A-possible-set
(def terminal-possible-products-and-sums (filter (fn[[[x y] product sum]] (and (new-A-possible-set product)(possible-sums sum))) possible-products-and-sums))

terminal-possible-products-and-sums ;; ([(4 13) 52 17])

;;Therefore the only possible values must have been x=4, y=13, and the sum was 17, and the product was 52

;; This little function factorises n
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


;; Suppose x=4, y=13:

;; From A's point of view:
;; A knows the product is 52, thinks x,y are either 4,13 or 2,26. Therefore he knows that B knows the sum is either 17 or 28

;; A can say "I don't know"

;; If B knows the sum is 17, then B doesn't know the answer, 
;; but B knows that x,y are 2,15 or 3,14 or 4,13 or 5,12 or 6,11 or 7,10 or 8,9

;; Therefore B knows that A knows that the product is 30, 42, 52, 66, 70, or 72

(map factors '(30 42 52 66 70 72)) ;; ((2 3 5) (2 3 7) (2 2 13) (2 3 11) (2 5 7) (2 2 2 3 3))

;; If A has the product 30, then he doesn't know x and y (they could be 2,15 or 3,10 or 5,6)
;; If A has the product 42, then he doesn't know x and y (they could be 2,21 or 6,7)
;; etc...
;; If A has the product 72, then 2,36 or 4,18 or 8,9 or 6,12 or ....

;; So if the sum is 17, B doesn't know the answer, but he does know that A doesn't know.

;; B can say "I knew that you didn't know"

;; From A's point of view, 

;; if B knew that the sum was 17, then that would be ok
;; but if B knew that the sum was 28

;; then B would know that the numbers were 2,26 or 3,25, or .... 14,14

(map #(list % (- 28 %)) (range 2 15)) ;;((2 26) (3 25) (4 24) (5 23) (6 22) (7 21) (8 20) (9 19) (10 18) (11 17) (12 16) (13 15) (14 14))
(map (fn[[x y]] (* x y)) '((2 26) (3 25) (4 24) (5 23) (6 22) (7 21) (8 20) (9 19) (10 18) (11 17) (12 16) (13 15) (14 14)))
 ;; (52 75 96 115 132 147 160 171 180 187 192 195 196)

;;Which means that B would know that A had one of
'(52 75 96 115 132 147 160 171 180 187 192 195 196)
;; as his product.

;; But if A had 115, then A would know that the answer had to be 
(factors 115) ;; (5 23)

;; So once A knows that B knows that A doesn't know, then A can rule out sum 28 in favour of sum 17

;; A now knows that the product is 52 and the sum is 17. That tells him that x=4, y=13

;; A says "Now I know"

;; Since B, knowing that the sum is 17, knows that A has 30, 42, 52, 66, 70 or 72
;; He can work out which of these would allow A to do the above.


;; But I don't know how he does that: 
;; Here's an attempt, but it just flails.



(map (fn [a] (map factors (map (fn[[x y]] (* x y)) (map #(list % (- a %)) (range 2 (int (inc (/ a 2)))))))) '(30 42 52 66 70 72))

;; 30: ((2 2 2 7) (3 3 3 3) (2 2 2 13) (5 5 5) (2 2 2 2 3 3) (7 23) (2 2 2 2 11) (3 3 3 7) (2 2 2 5 5) (11 19) (2 2 2 3 3 3) (13 17) (2 2 2 2 2 7) (3 3 5 5))


;; 42: ((2 2 2 2 5) (3 3 13) (2 2 2 19) (5 37) (2 2 2 3 3 3) (5 7 7) (2 2 2 2 17) (3 3 3 11) (2 2 2 2 2 2 5) (11 31) (2 2 2 3 3 5) (13 29) (2 2 2 7 7) (3 3 3 3 5) (2 2 2 2 2 13) (5 5 17) (2 2 2 2 3 3 3) (19 23) (2 2 2 5 11) (3 3 7 7))

;; 52 ((2 2 5 5) (3 7 7) (2 2 2 2 2 2 3) (5 47) (2 2 3 23) (3 3 5 7) (2 2 2 2 2 11) (3 3 43) (2 2 3 5 7) (11 41) (2 2 2 2 2 3 5) (3 13 13) (2 2 7 19) (3 5 37) (2 2 2 2 2 2 3 3) (5 7 17) (2 2 3 3 17) (3 11 19) (2 2 2 2 2 2 2 5) (3 7 31) (2 2 3 5 11) (23 29) (2 2 2 2 2 3 7) (3 3 3 5 5) (2 2 13 13))

;; 66 ((2 2 2 2 2 2 2) (3 3 3 7) (2 2 2 31) (5 61) (2 2 2 3 3 5) (7 59) (2 2 2 2 29) (3 3 3 19) (2 2 2 2 5 7) (5 11 11) (2 2 2 3 3 3 3) (13 53) (2 2 2 7 13) (3 3 5 17) (2 2 2 2 2 5 5) (7 7 17) (2 2 2 2 2 3 3 3) (19 47) (2 2 2 5 23) (3 3 3 5 7) (2 2 2 11 11) (23 43) (2 2 2 2 3 3 7) (5 5 41) (2 2 2 2 5 13) (3 3 3 3 13) (2 2 2 7 19) (29 37) (2 2 2 3 3 3 5) (5 7 31) (2 2 2 2 2 2 17) (3 3 11 11))

;; 70 ((2 2 2 17) (3 67) (2 2 2 3 11) (5 5 13) (2 2 2 2 2 2 2 3) (3 3 7 7) (2 2 2 2 31) (3 3 61) (2 2 2 3 5 5) (11 59) (2 2 2 3 29) (3 13 19) (2 2 2 2 7 7) (3 5 5 11) (2 2 2 2 2 3 3 3) (17 53) (2 2 2 3 3 13) (3 17 19) (2 2 2 5 5 5) (3 7 7 7) (2 2 2 2 2 3 11) (23 47) (2 2 2 2 3 23) (3 3 5 5 5) (2 2 2 11 13) (3 3 3 43) (2 2 2 3 7 7) (29 41) (2 2 2 2 3 5 5) (3 13 31) (2 2 2 2 2 2 19) (3 11 37) (2 2 2 3 3 17) (5 5 7 7))

;; 72 ((2 2 5 7) (3 3 23) (2 2 2 2 17) (5 67) (2 2 3 3 11) (5 7 13) (2 2 2 2 2 2 2 2 2) (3 3 3 3 7) (2 2 5 31) (11 61) (2 2 2 2 3 3 5) (13 59) (2 2 7 29) (3 3 5 19) (2 2 2 2 2 2 2 7) (5 11 17) (2 2 3 3 3 3 3) (19 53) (2 2 2 2 5 13) (3 3 7 17) (2 2 5 5 11) (7 7 23) (2 2 2 2 2 2 2 3 3) (5 5 47) (2 2 13 23) (3 3 3 3 3 5) (2 2 2 2 7 11) (29 43) (2 2 3 3 5 7) (31 41) (2 2 2 2 2 2 2 2 5) (3 3 11 13) (2 2 17 19) (5 7 37) (2 2 2 2 3 3 3 3)))

;; A can't have 30, or because there would be three possible sums whose products factorize uniquely

(map count 
     (map 
      (fn [a] 
        (filter #(= (count %) 2) 
                (map factors 
                     (map (fn[[x y]] (* x y)) 
                          (map #(list % (- a %)) 
                               (range 2 (inc (Math/floor (/ a 2)))))))))
          '(30 42 52 66 70 72)))

;; (3 4 3 6 5 6)













