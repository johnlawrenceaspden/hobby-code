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

gen/nat ; #clojure.test.check.generators.Generator{:gen #function[clojure.test.check.generators/gen-fmap/fn--10834]}

;; they cannot be called
(gen/nat)

;; but they can be sampled
(gen/generate gen/nat)
;; repeatedly
(gen/sample gen/nat) ; (0 1 1 3 3 5 3 7 1 7) ; (0 1 1 2 1 2 1 6 2 4) ; (0 1 1 1 1 2 4 1 6 9)
(gen/sample gen/nat)


(gen/sample gen/boolean) ; (true false true false false true true true false true)

;; and combined
(gen/vector gen/boolean 3) ; #clojure.test.check.generators.Generator{:gen #function[clojure.test.check.generators/gen-bind/fn--10839]}

(gen/sample (gen/vector gen/boolean 3)) ; ([false false true] [true true true] [true false false] [true false false] [false true true] [false false false] [true false true] [true false false] [false false true] [false false false])







(require '[com.gfredericks.test.chuck :as chuck])

(require '[com.gfredericks.test.chuck.generators :as gen'])


(def thing (gen'/for [len gen/nat
                      bools (gen/vector gen/boolean len)]
                     [len bools]))

(gen/sample thing) ; ([0 []] [1 [false]] [1 [true]] [1 [false]] [4 [true false false false]] [1 [false]] [0 []] [2 [true true]] [5 [true true true true false]] [3 [true false false]])


(gen/sample (gen'/string-from-regex #"([☃-♥]{3}|B(A|OO)M)*")) ;  ; ("" "BAM" "" "" "☭☚♝" "☕♚♕BAM" "BOOM☮♝☆BAMBAM" "BAM♓☶♊☰☾☏" "☞☦♓♃☆♊BAM" "")

(gen/sample (gen'/string-from-regex #"ab*(c|d)")) ; ("ac" "abc" "abc" "abd" "abbbc" "abbd" "abbbbbbc" "abbbbbd" "abbbbc" "abbc")
