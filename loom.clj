(use 'loom.graph)
(use 'loom.io)
(use 'loom.alg)

(def g (graph [1 2][2 3]{3 [4] 5 [6 7]} 7 8 9))
(view g)

(def dg (digraph g))
(view dg)

(def wg (weighted-graph {:a {:b 10 :c 20} :c {:d 30} :e {:b 5 :d 5}}))
(view wg)

(def wdg (weighted-digraph [:a :b 10] [:a :c 20] [:c :d 30] [:d :b 10]))
(view wdg)

(use 'loom.gen)

(def rwg (gen-rand (weighted-graph) 10 20 :max-weight 100))
(view rwg)

(def fg (fly-graph :neighbors (range 68) :weight (constantly 77) :start 10))
fg
(view fg)

(nodes fg)