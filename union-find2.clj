;; Union-Find II

;; Union-Find is a data structure specifically designed for keeping
;; track of partitions and equivalence relations.

;; A partition is a division of a large set into smaller sets
;; It can also be viewed as an equivalence relation

;; a is joined to b, if and only if a and be are in the same subset in the partition.

;; Consider our set of cities

(def cities ["London" "Birmingham" "Sheffield" "Bristol" "Leeds" "Liverpool" "Manchester"])

;; Our second try at a structure will be to associate with every item a pointer and a vector
;; The pointer will point to the leader of the item's group (the 'canonical element')

;; The vector will record every item which points to this item, which we should be able to use 
;; to speed up the join operation.



;; Our initial union-find for our cities looks like

(defn make-union-find [elts]
  (apply hash-map (mapcat (fn[x][x [x [x]]]) elts)))

(def initial (make-union-find cities))

initial
;; {"Liverpool" ["Liverpool" ["Liverpool"]],
;;  "Sheffield" ["Sheffield" ["Sheffield"]],
;;  "Manchester" ["Manchester" ["Manchester"]], 
;;  "Birmingham" ["Birmingham" ["Birmingham"]], 
;;  "Bristol" ["Bristol" ["Bristol"]],
;;  "London" ["London" ["London"]], 
;;  "Leeds" ["Leeds" ["Leeds"]]}

;; To join Liverpool to London now, we must first decide which is to be the new leader (say Liverpool)
;; And then modify London's entry to be 
;;  "London" ["Liverpool" []]
;; And Liverpool's to be
;; "Liverpool" ["Liverpool" ["Liverpool" "London"]]

;; Here's our new, considerably more complex version of join:

(defn join [union-find a b]
  ;; find the leaders of both groups
  (let [[leader-a _] (union-find a)
        [leader-b _] (union-find b)]
    ;; if a and b are in the same group
    (if (= leader-a leader-b) union-find ;; nothing to do
        ;; but otherwise
        (let [[_ followers-a] (union-find leader-a)
              [_ followers-b] (union-find leader-b)]
          (let [new-followers (into followers-a followers-b)] ;; combine the follower-lists
            (let [uf (assoc union-find leader-a [leader-a new-followers])] ;; add the new followers to the 'a group leader'
              (reduce (fn[uf x] (assoc uf x [leader-a []])) uf followers-b))))))) ;; and change every member of the 'b group' to follow the 'a group leader' instead


(join initial "Liverpool" "London")
;; {"Liverpool" ["Liverpool" ["Liverpool" "London"]],
;;  "Sheffield" ["Sheffield" ["Sheffield"]],
;;  "Manchester" ["Manchester" ["Manchester"]],
;;  "Birmingham" ["Birmingham" ["Birmingham"]],
;;  "Bristol" ["Bristol" ["Bristol"]],
;;  "London" ["Liverpool" []],
;;  "Leeds" ["Leeds" ["Leeds"]]}

        
;; I can't believe I just wrote that and it worked!

;; Checking whether things are joined is still easy
(defn joined? [union-find a b]
  (= (first (union-find a)) (first (union-find b))))


(def testnet
  (-> initial 
      (join "Liverpool" "London")
      (join "London" "Liverpool")
      (join "Bristol" "Manchester")
      (join "Liverpool" "Leeds")
      (join "Bristol" "Leeds")))

testnet
;; {"Liverpool" ["Bristol" []],
;;  "Sheffield" ["Sheffield" ["Sheffield"]],
;;  "Manchester" ["Bristol" []],
;;  "Birmingham" ["Birmingham" ["Birmingham"]],
;;  "Bristol" ["Bristol" ["Bristol" "Manchester" "Liverpool" "London" "Leeds"]],
;;  "London" ["Bristol" []],
;;  "Leeds" ["Bristol" []]}

;; I still don't actually believe that this is working:

(joined? testnet "Liverpool" "Liverpool") ;-> true
(joined? testnet "Liverpool" "London") ;-> true
(joined? testnet "Leeds" "London") ;-> true
(joined? testnet "Bristol" "London") ;-> true
(joined? testnet "Bristol" "Manchester") ;-> true
(for [x cities] (joined? testnet "Sheffield" x)) ;-> (false false true false false false false)

cities ;-> ["London" "Birmingham" "Sheffield" "Bristol" "Leeds" "Liverpool" "Manchester"]
(map (comp count (partial filter identity)) 
     (partition (count cities) 
                (for [x cities y cities] (joined? testnet x y)))) ;-> (5 1 1 5 5 5 5)

;; But I can't break it!


;; Now, neat as this all is, it's still possible that a join will
;; produce as many updates as there are elements in the partition

;; So I want to introduce one final optimization
;; When two groups are joined, the group that gets the updates should be the smaller group.

;; This doesn't sound like much, but it is actually crucial, reducing the time to 
;; join everything together from n^2 to n log n 

;; That's because the big group merges don't happen very often. Look at it from an element's point of view.
;; How many times does it expect to participate in a merge that changes its leader?

;; Unfortunately this makes join almost unimaginably complex:
(defn join [union-find a b]
  (let [[leader-a _] (union-find a)
        [leader-b _] (union-find b)]
    (if (= leader-a leader-b) 
      union-find ;; nothing to do
      (let [[_ followers-a] (union-find leader-a)
            [_ followers-b] (union-find leader-b)]
        (if (>= (count followers-a) (count followers-b))  ;; if a's in the bigger group
          (let [new-followers (into followers-a followers-b)] ;; combine follower-lists
            (let [uf (assoc union-find leader-a [leader-a new-followers])] ;; add the new followers to the 'a group leader'
              (reduce (fn[uf x] (assoc uf x [leader-a []])) uf followers-b))) ;; and change every member of the 'b group' to follow the 'a group leader' instead
          (join union-find b a)))))) ;; but if a's in the smaller group do it the other way round

;; I have a feeling that I should try to factor this function a bit. 
;; But I'm so proud of the fact that I got it working that I don't want to mess it up.
;; I think this feeling makes me what used to be called in my C days 'a three star programmer'.

;; And now I really do feel paranoid
(defn sanity-check [union-find cities]
  (map (comp count (partial filter identity)) 
       (partition (count cities) 
                  (for [x cities y cities] (joined? union-find x y)))))

cities ;-> ["London" "Birmingham" "Sheffield" "Bristol" "Leeds" "Liverpool" "Manchester"]
(sanity-check initial cities) ;-> (1 1 1 1 1 1 1)
(sanity-check (-> initial (join "Liverpool" "London")) cities) ;-> (2 1 1 1 1 2 1)
(sanity-check (-> initial (join "London" "Liverpool")) cities) ;-> (2 1 1 1 1 2 1)
(sanity-check (-> initial (join "London" "Liverpool")(join "Liverpool" "London")) cities) ;-> (2 1 1 1 1 2 1)
(sanity-check (-> initial (join "London" "Liverpool")(join "Bristol" "Manchester")(join "Liverpool" "London")) cities) ;-> (2 1 1 2 1 2 2)
(sanity-check (-> initial (join "Bristol" "London")(join "London" "Liverpool")(join "Bristol" "Manchester")(join "Liverpool" "London")) cities) ;-> (4 1 1 4 1 4 4)
(sanity-check (-> initial (join "Sheffield" "Birmingham")(join "Bristol" "London")(join "London" "Liverpool")(join "Bristol" "Manchester")(join "Liverpool" "London")) cities) ;-> (4 2 2 4 1 4 4)

;; Hmm, maybe I'm convinced.

;; This is actually a terribly old-fashioned implementation of union-find 

;; A quick google turns up: https://github.com/jordanlewis/data.union-find
;; Which looks spiffy, but which I haven't actually tried.

;; I want to use mine, dammit!

