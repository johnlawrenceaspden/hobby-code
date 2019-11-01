"123(45)(67)151" -> "c4d4e4f8g8a8b8c4g4c4"

"123(45)(67)151" 4 "0" ""
"23(45)(67)151"  4 "1"  ""
"3(45)(67)151"   4 "2"  "c4"
"(45)(67)151"   4 "3"  "c4d4"

(def keytrans {\0 "r" \1 "c" \2 "d" \3 "e" \4 "f" \5 "g" \6 "a" \7 "b"})

(defn it[{:keys [input, duration, current, curdur, output]}]
  (let [c (first input) s (rest input)]
    (cond ((set (seq "01234567")) c)
          {:input s, :duration duration, :current c, :curdur duration :output (str output  " " (keytrans current) curdur )}
          ((set (seq "-")) c)
          {:input s, :duration duration, :current current, :curdur duration   :output (str output  " " (keytrans current) curdur "~")}
          ((set (seq "(")) c)
          {:input s, :duration (* duration 2), :current current, :curdur curdur   :output output}
          ((set (seq ")")) c)
          {:input s, :duration (/ duration 2), :current current, :curdur curdur   :output output}
          :else
          {:input s, :duration duration, :current current, :curdur curdur   :output output}
    )))


  (def song "123-(45)(67)105-1--2(-3)00")

(def init {:input (seq song) :duration 4 :current \0 :curdur 0 :output ""})

(it init) 
(it (it init))
(it( it (it init)))
(it (it( it (it init))))
(it (it (it( it (it init)))))
(it (it (it (it( it (it init))))))
(it (it (it (it (it( it (it init)))))))
(it (it (it (it (it (it( it (it init))))))))
(last (take-while #(> (count (:input %)) 0) (iterate it init)))
(clojure.pprint/pprint (iterate it init))
