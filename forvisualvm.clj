(defn microbench[^long N ^long maxcount]

  (set! *unchecked-math* true)
  (set! *warn-on-reflection* true)

  (def a (long-array N))
  (def b (long-array N))
  
  (let [a ^longs a]
    (dotimes [i N] (aset a i i)))

  (time 
   (let [a ^longs a b ^longs b ]
     (loop [count 0 sum 0]
       (if (= count maxcount) sum
           (do 
             (println count sum)
             (dotimes [i N] (aset b i (unchecked-add (aget a i)(aget b i))))
             (recur (inc count) (unchecked-add sum (loop [i 0 ret 0] 
                                                     (if (= i N) ret
                                                         (recur (unchecked-inc i) (unchecked-add ret (aget b i)))))))))))))
