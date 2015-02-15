;; Destructuring Clojure's Maps

;; I can never ever remember how this works, so here is a note to self:

((fn [{a :a}] a) {:a 1}) ; 1

;; And by let-lambda isomorphism

(let [{a :a} {:a 1}] a) ; 1

;; Why on earth is the syntax the wrong way round? Why can't {:a a} match {:a 1}?

;; Similarly

((fn [{a :a b :b}] [a b]) {:a 1 :b 2}) ; [1 2]

(let [{a :a b :b} {:a 1 :b 2}] [a b]) ;  ; [1 2]

;; And with the common pattern where the variables are like the keys:

((fn [{:keys [a b]}] [a b]) {:a 1 :b 2}) ; [1 2]

(let [{:keys [a b]} {:a 1 :b 2}] [ a b ]) ; [1 2]


;; We can destructure recursively (although we may not be wise to if we keep forgetting how it works!)

((fn [{a :a {c :c d :d} :b}] [a c d]) {:a 1 :b {:c 2 :d 3}}) ; [1 2 3]

(let [{a :a {c :c d :d} :b} {:a 1 :b {:c 2 :d 3}}] [a c d]) ; [1 2 3]

;; And we can remember the keys entire on which we have recursed, so:

(let [{a :a {c :c d :d :as b} :b}
      {:a 1 :b {:c 2 :d 3}}]
  [a b c d]) ;-> [1 {:c 2, :d 3} 2 3]


;; Finally a 'real' example, a ring request map containing parameters and a session, both of
;; which have substructure

(def ring-request
  {:params {:action "a" :key "k" :spurious "sp"}
   :session {:data "d" :state "s"}
   :irrelevant "irr"})

;; So the parameters we're interested in look like
{:params {:action :key} :session {:data :state}}

;; And we can extract all the pieces, naming each part, like so:

(defn process-request [{{action :action key   :key   :as params } :params
                        {data   :data   state :state :as session} :session :as request}]
  (println action)
  (println key)
  (println data)
  (println state)
  (println params)
  (println session)
  (println request))

(process-request ring-request)
;; a
;; k
;; d
;; s
;; {:key k, :action a, :spurious sp}
;; {:state s, :data d}
;; {:irrelevant irr, :params {:key k, :action a, :spurious sp}, :session {:state s, :data d}}
