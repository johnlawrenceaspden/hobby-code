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

;; For all the possible x and y, what are all the possible sums and products?

(def possible-products-and-sums (map (fn [[x y]] [(list x y) (* x y) (+ x y)]) (xylist 100)))

(count possible-products-and-sums) ;;5050            Sanity check: 1+2+....+100=100*101/2

(take 10 possible-products-and-sums)   ;;([(1 1) 1 2] [(1 2) 2 3] [(1 3) 3 4] [(1 4) 4 5] [(1 5) 5 6] [(1 6) 6 7] [(1 7) 7 8] [(1 8) 8 9] [(1 9) 9 10] [(1 10) 10 11])

;; Let's collect together all the x,y which make a particular product:
(def sums-indexed-by-product (reduce (fn [map [[x y] product sum]] (assoc map product (cons sum (get map product '())))) {} possible-products-and-sums))

(take 10 sums-indexed-by-product)      ;;([1024 (64 80)] [2048 (96)] [3072 (112 128)] [4096 (128)] [5120 (144)] [6144 (160)] [9216 (192)] [32 (12 18 33)] [1056 (65 68 70 82 100 107)] [2080 (92 97 106)])
;; 1024 is 2*512, 4*256, 8*128, 16*64, or 32*32, of which only 16*64 and 32*32 are legal, summing to 80 and 64. If we know the product is 1024 then we have two choices for x and y.
;; 2048 is 2*1024, 4*512, 8*256, 16*128, or 32*64, of which only 32*64 is legal. So if we know the product is 2048 then we know x and y.

(count sums-indexed-by-product) ;;2906. There are 2906 distinct possibilities for the product

;; Similarly, let's take all the x,y which produce a particular sum
(def products-indexed-by-sum (reduce (fn [map [[x y] product sum]] (assoc map sum (cons product (get map sum '())))) {} possible-products-and-sums))

(first products-indexed-by-sum) ;; ([32 (256 255 252 247 240 231 220 207 192 175 156 135 112 87 60 31)])
;; sum 32 could be 1+31, 2+30, 3+29 ..., whose products are 31, 60, 87 ...

(count products-indexed-by-sum) ;; 199 There are 199 possible sums, from 1+1 to 100+100

;; A knows the product, but that doesn't give him a unique x,y, hence a unique sum. Therefore A's possibilities are all the products which are associated with more than one sum:

(def A-possibles (filter (fn[[product lst]] (< 1 (count lst))) sums-indexed-by-product))

(count A-possibles) ;; 1123 cut down from 2906
(take 10 A-possibles) ;; ([1024 (64 80)] [3072 (112 128)] [32 (12 18 33)] [1056 (65 68 70 82 100 107)] [2080 (92 97 106)] [4128 (134 139)] [7200 (170 171 172)] [64 (16 20 34 65)] [1088 (66 81 84)] [2112 (92 97 98 112 118)])

;; Just the possible products, without their associated possible sums
(def A-possible-set (set (map first A-possibles)))

(count A-possible-set)   ;;1123
(take 10 A-possible-set) ;;(1024 3072 32 1056 2080 4128 7200 64 1088 2112)

;; B already knew that A had one of these numbers. Therefore his sum must be one that is only produced by x,y that produce products in A's possible set.

(def B-possibles (filter (fn[[sum product-list]] (every? A-possible-set product-list)) products-indexed-by-sum))
(first B-possibles) ;; [35 (306 304 300 294 286 276 264 250 234 216 196 174 150 124 96 66 34)]
;;35 is a possible sum because every associated product is in the set where A has more than one choice of answer.

(def B-possible-set (apply sorted-set (map first B-possibles)))

(count B-possible-set) ;; 24 cut down from 199.

B-possible-set ;; #{5 7 9 10 11 13 15 16 17 19 21 23 25 27 29 31 35 37 41 43 45 47 49 53}

(map (fn [[sum product-list]] [sum (count product-list)]) B-possibles)

;; So the fact that A can't tell from the product somewhat restricts the possible products, and the fact that B knew that A had one of those products restricts the possible sums.

;; Now, we can restrict our considerations to only those x,y which can produce numbers in these restricted sets.
(def new-possible-products-and-sums (filter (fn[[[x y] product sum]] (and (A-possible-set product)(B-possible-set sum))) possible-products-and-sums))

(count new-possible-products-and-sums) ;;303

;; Our possible x,y have gone from 5050 to 303 pairs.

(take 10 new-possible-products-and-sums)([(1 4) 4 5] [(1 6) 6 7] [(1 8) 8 9] [(1 9) 9 10] [(1 10) 10 11] [(1 12) 12 13] [(1 14) 14 15] [(1 15) 15 16] [(1 16) 16 17] [(1 18) 18 19])

;; Let's recalculate what we can say about the sum given the product
(def new-products-indexed-by-sum (reduce (fn [map [[x y] product sum]] (assoc map sum (cons product (get map sum '())))) {} new-possible-products-and-sums))

;; and what we can say about the product given the sum
(def new-sums-indexed-by-product (reduce (fn [map [[x y] product sum]] (assoc map product (cons sum (get map product '())))) {} new-possible-products-and-sums))

(first new-products-indexed-by-sum) ;; [35 (306 304 300 294 286 276 264 250 234 216 196 174 150 124 96 66 34)]
(first new-sums-indexed-by-product) ;; [64 (16)]

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

;; #{4 8 9 15 21 25 38 39 55 56 58 63 64 68 76 80 82 86 88 92 94 96 98 104 112 124 136 140 144 148 152 160 162 164 170 172 174 176 182 184 186 200 204 208 216 222 228 232 246 250 258 266 270 276 282 288 296 304 310 324 328 336 340 348 350 352 364 370 372 374 378 400 406 408 414 416 430 432 434 444 456 460 464 468 476 480 486 490 492 494 496 500 504 506 520 522 528 532 540 544 550 552 558 580 588 592 594 598 600 612 630 646 660 672 682 690 696 700 702}

(count new-A-possible-set) ;; 119 (down from 193, down from 1123)

(def final-possible-products-and-sums (filter (fn[[[x y] product sum]] (and (new-A-possible-set product)(B-possible-set sum))) possible-products-and-sums))

(count final-possible-products-and-sums)  ;; 119 (now there are only 119 possibilities


(def final-sums-indexed-by-product (reduce (fn [map [[x y] product sum]] (assoc map product (cons sum (get map product '())))) {} final-possible-products-and-sums))
(def final-products-indexed-by-sum (reduce (fn [map [[x y] product sum]] (assoc map sum (cons product (get map sum '())))) {} final-possible-products-and-sums))

final-products-indexed-by-sum ;;{35 (304 276 250 216 174 124 96), 5 (4), 37 (340 336 270 232 186 160), 9 (8), 41 (414 408 400 378 364 348 310 288 148), 10 (25 21 9), 43 (460 456 432 406 372 352 222 82), 45 (506 504 500 494 486 476 464 434 416 374 350 324 296 266 200 164 86), 15 (56), 47 (552 550 540 532 522 496 480 370 246 172), 16 (64 63 55 39 15), 49 (600 598 594 588 580 558 544 528 490 468 444 328 258 94), 19 (88), 21 (104 98 80 68 38), 53 (702 700 696 690 682 672 660 646 630 612 592 520 492 430 282), 23 (112 76), 25 (144 136), 27 (182 176 170 162 152 140 92), 29 (208 204), 31 (228 184 58)}

(count final-sums-indexed-by-product) ;;119
(count final-products-indexed-by-sum) ;;20

(map (fn[[sum plist]] (count plist)) final-products-indexed-by-sum)

(def possible-sums (set (map first (filter (fn[[sum plist]] (= 1 (count plist))) final-products-indexed-by-sum))))

;; The sum must have been one of (5 9 15 19) in order for B to work it out.
;; The product must have been in new-A-possible-set

(def terminal-possible-products-and-sums (filter (fn[[[x y] product sum]] (and (new-A-possible-set product)(possible-sums sum))) possible-products-and-sums))

;; terminal-possible-products-and-sums
;; ([(1 4) 4 5] [(1 8) 8 9] [(7 8) 56 15] [(8 11) 88 19])

;; Hmm, why isn't there only one?

Suppose x=1, y=4:

From A's point of view:
A knows the product is 4, thinks x,y are either 1,4 or 2,2. Therefore he knows that B knows the sum is either 5 or 4

If B knows the sum is 5, then B doesn't know the answer, but B knows that x,y are either 1,4 or 2,3.
Therefore B knows that A knows that the product is 4 or 6.

If A has the product 4, then he doesn't know x and y (they could be 1,4 or 2,2)

If A has the product 6, then he doesn't know x and y (they could be 1,6 or 2,3)

So if the sum is 5, B doesn't know the answer, but does know that A doesn't know.

If B knows the sum is 4, then he doesn't know the answer, but he knows that x,y are either 1,3 or 2,2
Therefore he knows that A has the product 4 or 3.
If A knows the product is 4, then he doesn't know x or y (they could be 1,4 or 2,2)
But if A knows the product is 3, then he knows x=1 and y=3.

So if the sum is 4, B doesn't know the answer, but he knows that A may or may not know.

A says "I don't know"

This doesn't tell B anything. He already knows this. So does A. A might as well not have bothered saying it.

B says "I don't know, but I already knew that you didn't know"

This is enough to tell A which world they're in. B must know the sum is 5.

A now knows the sum is 5 and the product is 4. Therefore A knows that x,y are 1,4.

A says "I do know now"

From B's point of view:

B knows the sum is     5, thinks x,y are either 1,4 or 2,3. Therefore he thinks A knows the product is either 4 or 6 
B knows that A doesn't know.

A says "I don't know"

That makes no difference to B

B says "I don't know, but I already knew that you didn't know."












"I don't know"
B knows sum is 5, therefore either x=1,y=4, product is 4, A thinks 1,4 or 2,2.
                            or     x=2,y=3, product is 6, A thinks 1,6 or 2,3.
"I knew you didn't know", "I don't know"

A thinks that B has either 5 or 4. If B had 5, he would speak as above.
If B had 4, then he'd think that x and y could be 1 and 3, so the product would be 3, so A would know the answer.

A can deduce that B has 5, and knows that the product is 4. The only way this can be is if x=1, y=4.
"I know what they are"

B knows that the sum is 5, therefore either x=1, y=4 product=4, and A can reason as above
                                     or     x=2, y=3 product=6

If the product was 6, then A would think x=2,y=3 sum=5 if B had 5, he would speak as above. 
                      or   A would think x=1,y=6 sum=7

If B had the sum as 7, then he would think either x=1,y=6 => product 6, A can think 1,6 or 2,3
                                               or x=2,y=5 => product 10, A can think 1,10 or 2,5
                                               or x=3,y=4 => product 12, A can think 1,12 or 2,6, or 4,3












;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use 'clojure.test)

;; Firstly, let's make the 100 a variable, n. Then we can play with different size versions of the problem.

(def n 100)

;; That makes all the possible x and y:
(defn xylist [n]
  (for [x (range 1 (inc n)) y (range x (inc n))] [x y]))


;; At the beginning of the problem, 

;; A can't work out the value of x and y
;; B knows that A can't.

;; Obviously, if the number A was given is 3, then x and y can only be 1 and 3. Which numbers can be got more ways than one?

;; Which numbers can be got, and how many ways
(def sorted-frequencies (sort-by second (frequencies (map (fn [[x y]] (* x y)) (xylist 100)))))

(count sorted-frequencies) ;;There are 2096 possible products

(defn frequency-less-than [c]
  (fn [[k f]] (< f c)))

(count (take-while (frequency-less-than 2) sorted-frequencies)) ;; 1783 of which can only be made one way
(take 10 (sort (map first (take-while (frequency-less-than 2) sorted-frequencies)))) (1 2 3 5 7 11 13 17 19 23 ...) ;;obviously all the primes
(take 10 (drop 20 (sort (map first (take-while (frequency-less-than 2) sorted-frequencies))))) (.... 71 73 79 83 89 97 106 111 115 118 ....) ;; but 106 is not prime.
;; 106=2*53 => x=2 y=53

(def two-way-products (sort (map first (drop-while (frequency-less-than 2) sorted-frequencies ))))

(count two-way-products) ;; There are 1123 ambiguous products that A could have.
(take 10 two-way-products) ;; (4 6 8 9 10 12 14 15 16 18 ... ) 4=1*4, or 2*2    18=1*18,2*9 or 6*3

;; A must have one of the two-way-products

;; But for B to know that A didn't know, the sum alone must be enough to tell him that all possible x and y values lead to ambiguous products

;; If B knows the sum, the possible x and y are x= 1 to sum/2, y=sum - x

(defn xy-given-sum [n sum]
  (filter (fn [[x y]] (and (<= x n) (<= y n))) (for [x (range 1 (/ (+ sum 1) 2))] [x (- sum x)])))

(defn possible-products-given-sum [n sum] (map (fn[[x y]](* x y)) (xy-given-sum n sum)))

;; If B knows that the sum is 50, then A could have any of:
(possible-products-given-sum 100 50)
(49 96 141 184 225 264 301 336 369 400 429 456 481 504 525 544 561 576 589 600 609 616 621 624 625)
;; But if the product were 141 = 3*47, then A would know immediately that x=3 y=47

;; So for any given sum, we have to check that every possible product is two way
(defn two-way-products-given-sum [n sum]
  (map (set two-way-products) (possible-products-given-sum n sum)))

;; This map makes every unambiguous product nil
(two-way-products-given-sum 100 50)
(49 96 nil 184 225 264 nil 336 nil 400 429 456 nil 504 525 544 561 576 nil 600 609 616 621 624 nil)

;; So if there are no nils, then B, having that sum, would know that A couldn't find x and y
(defn no-unambiguous-products [n sum] 
  (not (some nil? (map (set two-way-products) (possible-products-given-sum n sum)))))

;; Possible sums go from 2 to 200
(filter (fn[sum] (not (ambiguous-sum 100 sum))) (range 2 201))

;; So B must have one of the numbers:
(5 7 9 10 11 13 15 16 17 19 21 23 25 27 29 31 35 37 41 43 45 47 49 53)

;; eg, if B has 16, then A must have one of 15, 28, 39, 48, 55, 60, 63, or 64
;; 15 could be 1*15 or 3*5, 28 could be 1*28 or 2 * 14, 55 could be 5*11, etc. A wouldn't be able to tell.

;; But if B has 54, then A could have any one of (possible-products-given-sum 100 54)
(53 104 153 200 245 288 329 368 405 440 473 504 533 560 585 608 629 648 665 680 693 704 713 720 725 728 729)
(two-way-products-given-sum 100 54)
(nil 104 153 200 245 288 nil 368 405 440 nil 504 nil 560 585 608 nil 648 665 680 693 704 nil 720 nil 728 729)
;; So B could have 53, which can only be 1*53

;; A has said "I don't know", so he has one of the two-way-products
;; B has said "I know you don't", so he has one of (5 7 9 10 11 13 15 16 17 19 21 23 25 27 29 31 35 37 41 43 45 47 49 53)

;; Now B says "I don't know them either", so B's information, combined with the knowledge that A has a two way product, can't be enough

;; If B had 5, then x,y could be 1+4, or 2+3. => A has 4 or 6
(possible-products-given-sum 100 5)

(defn no-of-possibilities-given [n sum]
  (count (clojure.set/intersection (set (possible-products-given-sum n sum)) (set two-way-products))))

(map (fn [sum] (no-of-possibilities-given 100 sum)) '(5 7 9 10 11 13 15 16 17 19 21 23 25 27 29 31 35 37 41 43 45 47 49 53)))

;; That doesn't seem to rule anything out. 

;; So, A knows the product, which must be a two way product.

;; Let's list all the possible numbers A can have, along with how they might be made
(def A-possibles (map (fn [[x y]] [(* x y) (list '* x y)]) (xylist 100)))

(take 20 A-possibles) ([1 (* 1 1)] [2 (* 1 2)] [3 (* 1 3)] [4 (* 1 4)] [5 (* 1 5)] [6 (* 1 6)] [7 (* 1 7)] [8 (* 1 8)] [9 (* 1 9)] [10 (* 1 10)] [11 (* 1 11)] [12 (* 1 12)] [13 (* 1 13)] [14 (* 1 14)] [15 (* 1 15)] [16 (* 1 16)] [17 (* 1 17)] [18 (* 1 18)] [19 (* 1 19)] [20 (* 1 20)])

;; We want to make a map from this list into all the possible x y


function to add [1 (* 1 1)] to the empty map to give {1 '((* 1 1))}

(defn addfn [map [product [times x y]]]
  (let [lst (get map product '())]
    (assoc map product (cons (list '* x y) lst))))


(def products-and-xys (reduce addfn {} A-possibles))

(take 10 products-and-xys)   ([1024 ((* 32 32) (* 16 64))] [2048 ((* 32 64))] [3072 ((* 48 64) (* 32 96))] [4096 ((* 64 64))] [5120 ((* 64 80))] [6144 ((* 64 96))] [9216 ((* 96 96))] [32 ((* 4 8) (* 2 16) (* 1 32))] [1056 ((* 32 33) (* 24 44) (* 22 48) (* 16 66) (* 12 88) (* 11 96))] [2080 ((* 40 52) (* 32 65) (* 26 80))])

(defn product-and-xys-to-product-and-sums [[product lst]] [product (map (fn [[times x y]] (+ x y)) lst)])

(product-and-xys-to-product-and-sums (first products-and-xys))

(def products-and-sums (map product-and-xys-to-product-and-sums products-and-xys))

(take 10 products-and-sums)










