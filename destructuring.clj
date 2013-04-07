;; Destructuring 

;; One of Clojure's great strengths is the ability to create maps ad-hoc and to deal with them easily.

;; You can create an ad-hoc structure with named fields:

{:a 3 :b 1}

;; And unpack it, providing defaults for its values if they are not present, as easily as:

(defn add-maps [{:keys [a b] :or {a 0 b 0} }]
  (+ a b))

(add-maps {:a 3 :b 1}) ;-> 4
(add-maps {:a 3}) ;-> 3
(add-maps {}) ;-> 0
(add-maps {:c "hello"}) ;-> 0

;; I love this and use it all the time. It's well worth learning the syntax.

;; Try playing with this expression:
((fn [{:keys[a b] :or {b 55} :as m}] {:a a :b b :m m}) {})





