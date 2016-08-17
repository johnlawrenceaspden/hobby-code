(ns blah
  (:require [com.stuartsierra.component :as component]))

;; First off, how do records work?

(defrecord MyType [a b])

;; gives you a constructor for free
(def foo (->MyType [1 2 3] [4 5 6]))

(def foo1 (map->MyType {:a [1 2 3], :b [4 5 6]}))

foo     ;;#blah.MyType{:a [1 2 3], :b [4 5 6]}
foo1    ;;#blah.MyType{:a [1 2 3], :b [4 5 6]}

(= foo foo1) ; true

(:a foo) ; [1 2 3]
(:b foo) ; [4 5 6]

;; Why is this better than {:a [1 2 3] :b [4 5 6]} ? 

(defn mytype-with-length [n]
  (let [a (vec (range n))
        b (vec (range n))] 
    (->MyType a b)))

(mytype-with-length 4) ;;#blah.MyType{:a [0 1 2 3], :b [0 1 2 3]}

;; doesn't seem to be particularly type-forcy
(assoc foo :c 'doom) ; #blah.MyType{:a [1 2 3], :b [4 5 6], :c doom}

;; but this returns a map, rather than a MyType. Maybe MyTypes always have a and b?
(dissoc foo :a) ; {:b [4 5 6]}

(assoc foo :a nil) ; #blah.MyType{:a nil, :b [4 5 6]}



(defprotocol Bazzer
  "This is an interface that states a `baz` method should be implemented"
  (baz [this] [a b])) ; you can define multi arity interface, but seemingly can't implement it on a defrecord?
                      ; instead use `extend-protocol` for those situations
                      ; see following example

(extend-protocol Bazzer
  Object ; the return type determines if symbols referenced (e.g. a and b) can be resolved
         ; if not defined (or the wrong type) then errors can occur
  (baz
    ([a] 1)
    ([a b] 2)))



(baz "any value and I'll return 1") ; 1        
(baz "any two values" "and I'll return 2") ; 2


(defrecord Foo [a b]
  Bazzer ; enforces the interface (but the error thrown without this defined, doesn't actually clarify)
  (baz [this] "Foo Bazzer")) ; associate a function with our `defrecord` map


(defrecord Bar [a b] Bazzer
  (baz [this] (str "Bar Bazzer -> " a " / " b)))

(def foo (->Foo :bar :baz)) ; #'blah/foo

(def bar (map->Bar {:a 1 :b 2})) ; #'blah/bar

bar ; #blah.Bar{:a 1, :b 2}
foo ; #blah.Foo{:a :bar, :b :baz}

(baz foo) ; "Foo Bazzer"
(baz bar) ; "Bar Bazzer -> 1 / 2"
(baz (map->Bar {:a 1 :b 2})) ; "Bar Bazzer -> 1 / 2"
(baz (map->Bar {:a 10 :b 2 :c 3})) ; "Bar Bazzer -> 1 / 2"












(defrecord Database [host port connection]
  component/Lifecycle

  )

