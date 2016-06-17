(in-ns 'user)

(require 'cemerick.pomegranate) ;; one ring to rule them all

(cemerick.pomegranate/add-dependencies ;; one ring to find them
 :repositories (merge cemerick.pomegranate.aether/maven-central {"clojars" "http://clojars.org/repo"})
 ;; one ring to bring them all
 :coordinates '[[org.clojure/test.check "0.9.0"]
                [com.gfredericks/test.chuck "0.2.6"]])

;; and in the darkness, bind them
(require '[clojure.test.check :as tc] 
         '[clojure.test.check.generators :as gen]
         '[clojure.test.check.clojure-test :refer [defspec]]
         '[clojure.test.check.properties :as prop])


;; We got generators

;; This one makes natural numbers
gen/nat ; #clojure.test.check.generators.Generator{:gen #function[clojure.test.check.generators/gen-fmap/fn--10834]}

;; they cannot be called
;; (gen/nat) ;; error

;; but they can be sampled
(gen/generate gen/nat) ; 26
;; repeatedly
(gen/sample gen/nat) ; (0 1 1 3 3 5 3 7 1 7) ; (0 1 1 2 1 2 1 6 2 4) ; (0 1 1 1 1 2 4 1 6 9)
;; as many times as you like
(gen/sample gen/boolean 10) ; (false false false true true true false false false false)

;; gen/vector is a function, not a generator 
gen/vector ; #function[clojure.test.check.generators/vector]

;; but it can be used to build generators
(gen/vector gen/boolean) ; #clojure.test.check.generators.Generator{:gen #function[clojure.test.check.generators/gen-bind/fn--10845]}

;; here we make vectors of booleans, of arbitrary length
(gen/sample (gen/vector gen/boolean)) ; ([] [] [true true] [true false true] [] [true true false false false] [true true false false true] [false true] [true true true] [false true true false false true true true true])

;; here are some of length 3
(gen/sample (gen/vector gen/boolean 3)) ; ([true true true] [true true true] [true true true] [false true false] [true true true] [true false false] [false false false] [true true false] [false true true] [true true true])

;; and there are many generators and ways of combining them
(gen/sample
 (gen/tuple
  gen/boolean
  gen/char
  gen/char-ascii
  (gen/vector gen/boolean 3))) ; ([true \¾ \m [false false false]] [true \} \u [true true false]] [true \È \> [false false false]] [true \ú \6 [false true true]] [true \8 \" [false false false]] [true \ \~ [false true true]] [false \= \= [true false true]] [true \¤ \Z [false true false]] [false \ï \u [true false false]] [false \backspace \e [false true false]])


;; OK, what can we do with them?

;; Here's an example straight from the docs

;; We say that sort should be idempotent on any vector of ints
(def sort-idempotent-prop
  (prop/for-all [v (gen/vector gen/int)]
                (= (sort v) (sort (sort v)))))

;; We can test this by generating 100 random vectors of ints and seeing if it always holds
(tc/quick-check 100 sort-idempotent-prop)
;; {:result true, :num-tests 100, :seed 1466100813479}

;; Another property of sort is that the list should be sorted
(def prop-sorted
   (prop/for-all [v (gen/not-empty (gen/vector gen/int))]
     (let [s (sort v)]
       (apply < s))))

;; Which is often true
(tc/quick-check 1 prop-sorted)
;; {:result true, :num-tests 1, :seed 1466100802862}

;; But occasionally not true
(tc/quick-check 10 prop-sorted)
;; {:result false,
;;  :seed 1466100766430,
;;  :failing-size 5,
;;  :num-tests 6,
;;  :fail [[-3 -3 -1 -3]],
;;  :shrunk
;;  {:total-nodes-visited 9, :depth 2, :result false, :smallest [[-3 -3]]}}

;; What's really nice here is that the original vector that failed was [-3 -3 -1 -3]
(apply < (sort [-3 -3 -1 -3])) ; false

;; But that quick check has managed to reduce that to a smaller test case
(apply < (sort [-3 -3])) ; false

;; And even better, our tests are repeatable!
(tc/quick-check 1 prop-sorted :seed 1466100766430) ; {:result true, :num-tests 1, :seed 1466100766430}

;; just kidding!
(tc/quick-check 6 prop-sorted :seed 1466100766430) ; {:result false, :seed 1466100766430, :failing-size 5, :num-tests 6, :fail [[-3 -3 -1 -3]], :shrunk {:total-nodes-visited 9, :depth 2, :result false, :smallest [[-3 -3]]}}

;; We're a Test Driven Development Shop here:

(def sort (comp distinct clojure.core/sort))

;; victory
(tc/quick-check 6 prop-sorted :seed 1466100766430)

;; but a true klingon subjects his code to the strictest QA procedures, which it invariably triumphantly passes.
(tc/quick-check 1000 prop-sorted) ; {:result true, :num-tests 1000, :seed 1466163332094}

;; The test-monkeys cotton on
(def prop-not-thrown-any-away
   (prop/for-all [v (gen/not-empty (gen/vector gen/int))]
     (let [s (sort v)]
       (= (count s)(count v)))))

(tc/quick-check 1000 prop-not-thrown-any-away)
;; {:result false,
;;  :seed 1466163668636,
;;  :failing-size 3,
;;  :num-tests 4,
;;  :fail [[0 2 2]],
;;  :shrunk
;;  {:total-nodes-visited 7, :depth 1, :result false, :smallest [[2 2]]}}

;; arse

(def sort (comp range count))

(tc/quick-check 1000 sort-idempotent-prop) ; {:result true, :num-tests 1000, :seed 1466163856476}
(tc/quick-check 1000 prop-not-thrown-any-away) ; {:result true, :num-tests 1000, :seed 1466163857441}
(tc/quick-check 1000 prop-sorted) ; {:result true, :num-tests 1000, :seed 1466163858255}

;; Now, we are become death.

;; So, how to make sure that our tests are run?

(use 'clojure.test)

(run-all-tests) ; {:test 0, :pass 0, :fail 0, :error 0, :type :summary}

(deftest test-test-f
  (is false))

(deftest test-test-t
  (is true))

(run-all-tests) ; {:test 3, :pass 1, :fail 2, :error 0, :type :summary}











(require '[com.gfredericks.test.chuck :as chuck])

(require '[com.gfredericks.test.chuck.generators :as gen'])


(def thing (gen'/for [len gen/nat
                      bools (gen/vector gen/boolean len)]
                     [len bools]))

(gen/sample thing) ; ([0 []] [1 [false]] [1 [true]] [1 [false]] [4 [true false false false]] [1 [false]] [0 []] [2 [true true]] [5 [true true true true false]] [3 [true false false]])


(gen/sample (gen'/string-from-regex #"([☃-♥]{3}|B(A|OO)M)*")) ;  ; ("" "BAM" "" "" "☭☚♝" "☕♚♕BAM" "BOOM☮♝☆BAMBAM" "BAM♓☶♊☰☾☏" "☞☦♓♃☆♊BAM" "")

(gen/sample (gen'/string-from-regex #"ab*(c|d)")) ; ("ac" "abc" "abc" "abd" "abbbc" "abbd" "abbbbbbc" "abbbbbd" "abbbbc" "abbc")







;;(ns test.core
;;  (:require [clojure.core.typed :as t]))

;; (defn foo
;;   "I don't do a whole lot."
;;   [x]
;;   (println x "Hello, World!"))


;; (require '[clojure.test.check :as tc])
;; (require '[clojure.test.check.generators :as gen])
;; (require '[clojure.test.check.properties :as prop])





;; (def wason-prop
;;   (prop/for-all [v (gen/not-empty (gen/vector gen/int))]
;;                 (or (apply <= v) (apply >= v))))

;; (tc/quick-check 3 wason-prop)

;; (def linear-prop
;;   (prop/for-all [v (gen/not-empty (gen/vector gen/int))]
;;                 (or (=(count v) 1)(apply = (map - v (rest v))))))

;; (tc/quick-check 5 linear-prop)

;; (clojure.test.check.clojure-test/defspec linearity 100 linear-prop)

(t/defalias NInts
  (t/U nil (t/Coll t/Int)))

(t/ann summarise
     (t/IFn [ NInts -> t/Int]
            [NInts t/Int -> t/Int ]))

(defn summarise
  ([nseq] (summarise nseq 0))
  ([nseq acc]
   (println nseq acc)
   (if (seq nseq)
                (* (summarise (next nseq) (inc acc)) (first nseq))
                42)))
