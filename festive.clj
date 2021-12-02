#!/usr/bin/env clojure

;; A Festive Theorem

;; make the repl stop after the first 20 elements of a sequence
(set! *print-length* 20)


;; If you square an even number, then it will be divisible by 4

(def evens (map #(* 2 %) (range))) ;; (0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 ...)

(defn square [x] (* x x))

(def squareevens (map square evens)) ;; (0 4 16 36 64 100 144 196 256 324 400 484 576 676 784 900 1024 1156 1296 1444 ...)

(def mod4 #(rem % 4))

(map mod4 squareevens) ;; (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ...)

;; But if you square an odd number, then it will leave a residue of 1 mod 4

(def odds (map #(+ (* 2 %) 1) (range))) ;; (1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 ...)









