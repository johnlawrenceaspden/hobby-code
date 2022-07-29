#!/usr/bin/env clojure

(println "hello")

;; using java builtins
(let [a (Math/sqrt 7)] (println a))

;; clojure standard library
(require '[clojure.string])
(println (clojure.string/upper-case "hello"))


