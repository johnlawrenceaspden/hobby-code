;; Requires [org.clojars.achim/multiset "0.1.0-SNAPSHOT"]
(require '[multiset.core :as ms])

;; The Randomized Contraction Algorithm

;; Consider a graph

;; A-B-C=D
;;     |
;;;    E

;; ( Note the double edge C=D )

;; We may, for all sorts of reasons, want to find minimum cuts in a graph.

;; A cut is a division of the graph's nodes into two, and its value is the number of edges between the two subsets

;; So a cut might be A,C vs B,D,E, which has crossing edges A-B,B-C,C-D,C-D again, and C-E, for a value of 5.

;; In order to make this cut with scissors, you'd have to cut 5 pieces of string.

;; It is, I claim, obvious that the minimal cuts of this graph have one crossing edge. An example cut would be A,B vs C,D,E.
;; Another would be E vs A,B,C,D.

;; The Randomized Contraction Algorithm is a beautiful way of finding such minimal cuts.

;; When you first see it, you cannot believe that it ever works.


;; Suppose we represent our graph.

;; A-B-C=D
;;     |
;;;    E

;; Which we might represent as

;; A : B
;; B : A C
;; C : B D D E
;; D : C C
;; E : C

;; Despite the redundancy, we will also keep a separate list of edges
;; A-B B-A B-C C-B C-D C-D C-E D-C D-C E-C

;; We'd like to consider 'contractions' of this graph. We pick
;; (uniformly at random) an edge, say the one from C-E, and merge the
;; two nodes it connects, throwing away any self-loops that this
;; creates

;; A-B-CE=D

;; This is easily represented by merging our C and E lists

;; A  : B
;; B  : A C
;; D  : C C
;; CE : B D D ( and throw away C and E from this list since they now represent self-loops)

;; And throwing away the edges E-C C-E from the edge list, (since they are now self loops)

;; A-B, B-A B-C C-B C-D C-D D-C D-C


;; Since we need to deal with multiple edges and nodes that are formed
;; by combining several nodes, I think that the most natural
;; representation of this data is as a set of vectors of sets and
;; multisets:
(def graph
  #{
    [ #{:A}, (ms/multiset :B)]
    [ #{:B}, (ms/multiset :A :C) ]
    [ #{:C}, (ms/multiset :B :D :D :E)]
    [ #{:D}, (ms/multiset :C :C) ]
    [ #{:E}, (ms/multiset :C) ]
    })

;; For the edge list, perhaps
(def edges (ms/multiset [:A,:B] [:B,:A] [:B,:C] [:C,:B] [:C,:D] [:C,:D] [:C,:E] [:D,:C] [:D,:C] [:E,:C]))

;; to represent our contraction, then we pick a random edge

(rand-nth (seq edges)) ;-> [:E :C]

;; take the two elements of the graph whose sets contain :E and :C

(def nodestomerge (filter (fn [[a,b]] (or (contains? a :E) (contains? a :C))) graph)) ;-> ([#{:C} #{:E :D :D :B}] [#{:E} #{:C}])

;; Combine them, removing self-loops

;; To do this I'd like to add a disj-all function which will clear an element from a multiset no matter how many times it's in there
(defn disj-all [edges edge]
  (if (contains? edges edge)
    (recur (disj edges edge) edge)
    edges))


(def newnode (let [[[aset aedges][bset bedges]] nodestomerge]
               [ (clojure.set/union aset bset) (disj-all (disj-all (ms/union aedges bedges) :C) :E)])) ;-> [#{:C :E} #{:B :D :D}]

;; And then put the new node into the old graph, removing the old ones

(def newgraph (-> graph
                  (disj (first nodestomerge))
                  (disj (second nodestomerge))
                  (conj newnode)))

;; #{[#{:C :E} #{:B :D :D}]
;;   [#{:A} #{:B}]
;;   [#{:D} #{:C :C}]
;;   [#{:B} #{:C :A}]}


;; We must also remove edges that are now self loops from our edge list

(def newedges (-> edges
               (disj-all  [:E :C])
               (disj-all  [:C :E])))

;; #{[:D :C] [:D :C] [:C :D] [:C :D] [:C :B] [:B :C] [:B :A] [:A :B]}

;; OK, turn the above into a function

(defn contraction [ [graph edges] [e1 e2] ]
  (let [nodestomerge (filter (fn [[a,b]] (or (contains? a e1) (contains? a e2))) graph)
        newnode (let [[[aset aedges][bset bedges]] nodestomerge]
                  [ (clojure.set/union aset bset) (disj-all (disj-all (ms/union aedges bedges) e1) e2)])
        newgraph (-> graph
                     (disj (first nodestomerge))
                     (disj (second nodestomerge))
                     (conj newnode))
        newedges (-> edges
                     (disj-all  [e1 e2])
                     (disj-all  [e2 e1]))]
    [newgraph, newedges]))

(contraction [graph edges] [:C :E])

;; [#{[#{:C :E} #{:B :D :D}]
;;    [#{:A} #{:B}]
;;    [#{:D} #{:C :C}]
;;    [#{:B} #{:C :A}]}
;;  #{[:D :C] [:D :C] [:C :D] [:C :D] [:C :B] [:B :C] [:B :A] [:A :B]}]


;; Compare with the hand-calculated version:

;; A  : B
;; B  : A C
;; D  : C C
;; CE : B D D

;; A-B, B-A B-C C-B C-D C-D D-C D-C


;; The randomized contraction algorithm, then, is to repeatedly contract the graph along a randomly chosen edge until we have only two nodes left.

(defn randomized-contraction [[graph edges]]
  (if ( <= (count graph) 2) [graph edges]
      (let [doomed-edge (rand-nth (seq edges))
            [newgraph newedges] (contraction [graph edges] doomed-edge)]
        (recur [newgraph newedges]))))


;; Let's run it a few times:
(randomized-contraction [graph edges]) ;-> [#{[#{:A :B} #{:C}] [#{:C :D :E} #{:B}]} #{[:C :B] [:B :C]}]
(randomized-contraction [graph edges]) ;-> [#{[#{:A :B} #{:C}] [#{:C :D :E} #{:B}]} #{[:C :B] [:B :C]}]
(randomized-contraction [graph edges]) ;-> [#{[#{:A :C :B :D} #{:E}] [#{:E} #{:C}]} #{[:E :C] [:C :E]}]
(randomized-contraction [graph edges]) ;-> [#{[#{:C :B :D :E} #{:A}] [#{:A} #{:B}]} #{[:B :A] [:A :B]}]
(randomized-contraction [graph edges]) ;-> [#{[#{:C :B :D :E} #{:A}] [#{:A} #{:B}]} #{[:B :A] [:A :B]}]
(randomized-contraction [graph edges]) ;-> [#{[#{:A :B} #{:C}] [#{:C :D :E} #{:B}]} #{[:C :B] [:B :C]}]
(randomized-contraction [graph edges]) ;-> [#{[#{:A :B} #{:C}] [#{:C :D :E} #{:B}]} #{[:C :B] [:B :C]}]
(randomized-contraction [graph edges]) ;-> [#{[#{:C :B :D :E} #{:A}] [#{:A} #{:B}]} #{[:B :A] [:A :B]}]
(randomized-contraction [graph edges]) ;-> [#{[#{:A :B} #{:C}] [#{:C :D :E} #{:B}]} #{[:C :B] [:B :C]}]
(randomized-contraction [graph edges]) ;-> [#{[#{:A :C :B :D} #{:E}] [#{:E} #{:C}]} #{[:E :C] [:C :E]}]
(randomized-contraction [graph edges]) ;-> [#{[#{:A :B} #{:C}] [#{:C :D :E} #{:B}]} #{[:C :B] [:B :C]}]
(randomized-contraction [graph edges]) ;-> [#{[#{:A :B} #{:C}] [#{:C :D :E} #{:B}]} #{[:C :B] [:B :C]}]
(randomized-contraction [graph edges]) ;-> [#{[#{:A :B} #{:C}] [#{:C :D :E} #{:B}]} #{[:C :B] [:B :C]}]




;; What we're actually interested in is the cut represented by the result
(defn cut [[graph edges]]
  (let [[[a _] [b _]] (seq graph)]
    [a b (/ (count edges) 2)]))


(cut (randomized-contraction [graph edges])) ;-> [#{:A :C :B :D} #{:E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:C :B :D :E} #{:A} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :B} #{:C :D :E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:C :B :D :E} #{:A} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:C :B :D :E} #{:A} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :B} #{:C :D :E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:C :B :D :E} #{:A} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :C :B :D} #{:E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :B} #{:C :D :E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:C :B :D :E} #{:A} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:C :B :D :E} #{:A} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :C :B :D} #{:E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :B} #{:C :D :E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :B} #{:C :D :E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :C :B :D} #{:E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:C :B :D :E} #{:A} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:C :B :D :E} #{:A} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :C :B :E} #{:D} 2]     <-- fail!
(cut (randomized-contraction [graph edges])) ;-> [#{:A :B} #{:C :D :E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :C :B :D} #{:E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:C :B :D :E} #{:A} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :B} #{:C :D :E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :C :B :D} #{:E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :C :B :D} #{:E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :C :B :D} #{:E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :C :B :D} #{:E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :B} #{:C :D :E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:A :B} #{:C :D :E} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:C :B :D :E} #{:A} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:C :B :D :E} #{:A} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:C :B :D :E} #{:A} 1]
(cut (randomized-contraction [graph edges])) ;-> [#{:C :B :D :E} #{:A} 1]


;; I find this algorithm rather amazing! See if you can work out why it works.

;; The best bound I know for its correctness is that it will find a given minimal cut in more than (/ (choose n 2)) times

;; So for our five element graph we should find a given minimal cut at least 1/10 th of the time.

;; Out of the thirty possible cuts, I can see three that are minimal, so the theoretical guarantee is that it will get a result
;; 30% of the time. It seems to be doing rather better than that!

;; Obviously, this graph is an easy case, but it seems to find each of the three minimal cuts 30% of the time, and when it fails,
;; it doesn't fail terribly:

(frequencies (take 1000 (repeatedly (fn[] (cut (randomized-contraction [graph edges]))))))
;; {[#{:C :B :D :E} #{:A} 1] 299,
;;  [#{:A :B} #{:C :D :E} 1] 295,
;;  [#{:A :C :B :D} #{:E} 1] 309,
;;  [#{:A :C :B :E} #{:D} 2] 97}


;; On a smug note, I heard about this algorithm yesterday (including probabilistic correctness proof),
;; and today spent an hour doing it by hand on paper before writing this program/essay straight through with no testing except that which is in the text here.
;; I hadn't used multisets before.
;; The program appears to be working reasonably well. Can anyone see/find a bug? Is it possible to understand it?

;; I therefore claim that clojure is not at all a hard language to use.

;; Does anyone fancy implementing this algorithm in a fast language to see what sort of speed penalty I'm paying here?

;; I mean obviously there's some sort of hundred-fold penalty for using dynamic typing, but apart from that.

;; I'm a bit nervous about having to turn the edge-set and node-set into seqs to filter on them and to choose randomly,
;; and I'm thinking that dooms this implementation to being O(n^2).
;; That seems far too much, since the algorithm is very easy to do on paper.

;; I think it should be possible to do this in O(n). I wonder if you need to be significantly more cunning to do that.
;; It occurs that a lookup-table of original-nodes to super-nodes would allow you to remove the filter step,
;; and there must be a way to choose randomly from a set without having to put it in order!
