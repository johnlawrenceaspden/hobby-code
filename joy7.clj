
;; anonymous recursive function
((fn fact [n]
   (if (< n 2) n
       (* n (fact (dec n))))) 10) ; 3628800

;; anonymous multi-arity function
((fn
   ([x] #{x})
   ([x y] #{x y})) 2) ; #{2}

;; anonymous variadic
((fn arity2+ [x y & z] [x y z]) 1 2 3 4) ; [1 2 (3 4)]

;; concise anonymous variadic
(#(list %1 %2 %3 %&) 1 2 3 4 5 6 7) ; (1 2 3 (4 5 6 7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def x 42)

;; gets printed out in the terminal where the swank server is running!
(.start (Thread. #(println "Answer: " x)))

(def y)

;y ; var user/y is unbound
;z ; unable to resolve symbol z


(defn fizzbuzz [start]
  (when (rem start 3) (print "fizz"))
  (when (rem start 5) (print "buzz"))
  (when (not (or (rem start 3) (rem start 5))) (print start))
  (println)
  (recur (inc start)))

                                        ;(fizzbuzz 0) ; don't do this


java.util.Locale/JAPAN ; #<Locale ja_JP>

(Math/sqrt 9) ; 3.0

(new java.util.HashMap {"foo" 42 "bar" 9 "baz" "quux"}) ; #<HashMap {baz=quux, foo=42, bar=9}>

(java.util.HashMap. {"foo" 42 "bar" 9 "baz" "quux"}) ; #<HashMap {baz=quux, foo=42, bar=9}>

(.x (java.awt.Point. 10 20)) ; 10

(.divide (java.math.BigDecimal. "42") 2M) ; 21M
(user/tryc (.divide (java.math.BigDecimal. "42") 2)) ; #<ClassCastException java.lang.ClassCastException: java.lang.Integer cannot be cast to java.math.BigDecimal>

(let [origin (java.awt.Point. 0 0)]
  (set! (.x origin) 15)
  (str origin)) ; "java.awt.Point[x=15,y=0]"

(.endsWith (.toString (java.util.Date.)) "2011") ; true

(.. (java.util.Date.) toString (endsWith "2010")) ; false

(doto (java.util.HashMap.)
  (.put "HOME" "/home/me")
  (.put "SRC" "src")
  (.put "BIN" "classes")) ; #<HashMap {HOME=/home/me, BIN=classes, SRC=src}>

(user/tryc (throw (Exception. "I done throwed"))) ; #<Exception java.lang.Exception: I done throwed>

(defn throw-catch [f]
  [(try
     (f)
     (catch ArithmeticException e "No dividing by zero!")
     (catch Exception e (str "You are so bad:" (.getMessage e)))
     (finally (println "returning...")))])

(throw-catch #(/ 10 5)) ; returning...
[2]

(throw-catch #(/ 10 0)) ; returning...
["No dividing by zero!"]

(throw-catch #(throw (Exception. "foo"))) ; returning...
["You are so bad:foo"]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns joy.ch1) ; nil

(defn hello[] (println "Hello")) ; #'joy.ch1/hello

(defn report-ns [] (str "ns" *ns*)) ; #'joy.ch1/report-ns

(report-ns) ; "nsjoy.ch1"

hello ; #<ch1$hello joy.ch1$hello@d7e89c>

(ns joy.another) ; nil

(joy.ch1/report-ns) ; "nsjoy.another"

(ns joy.req
  (:require clojure.set))

(ns joy.req-alias
  (:require [clojure.set :as s])) ; nil

(s/intersection #{1 2 3} #{3 4 5}) ; #{3}

;; namespace is not an identifier

;clojure.set ; error

java.lang.Object ; java.lang.Object


(ns joy.use-ex
  (:use [clojure.string :only [capitalize]]))

(map capitalize ["kilgore" "trout"])






