(def P {:000 1, :001 3, :010 3, :011 9, :100 3, :101 9, :110 9, :111 27})

(defn code [P] (for [[s p] P] [p s]))

(def C (code P))

C ; ([1 :000] [3 :001] [3 :010] [9 :011] [3 :100] [9 :101] [9 :110] [27 :111])

(defn huffman-combine [C]
  (if (< (count C) 2) C
      (let [SC (sort-by first C)
            a (first SC)
            b (second SC)]
        (cons [(+ (first a) (first b)) a b] (drop 2 SC)))))

(def HC (nth (iterate huffman-combine (code P)) (count P)))
(nth (iterate huffman-combine (code P)) (dec (dec (count P))))
HC ; ([64 [27 :111] [37 [18 [9 :011] [9 :101]] [19 [9 :110] [10 [4 [1 :000] [3 :001]] [6 [3 :010] [3 :100]]]]]])


(defn symbol-depths
  ([HC] (symbol-depths HC 0))
  ([HC n]
     (def-let [SHC (group-by count HC)
               symbols (SHC 2)
               subtrees (map #(drop 1 %) (SHC 3))]
       (if (empty? symbols) (mapcat #(#'symbol-depths % (inc n)) subtrees)
           (concat (list n (map second symbols)) (mapcat #(#'symbol-depths % (inc n)) subtrees) )))))

(cct/dotrace (symbol-depths) (symbol-depths  HC))
                  








  