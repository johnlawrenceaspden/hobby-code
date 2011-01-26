(import '(java.net URL))

(def irisdata  (map #(clojure.string/split % #",")
                    (clojure.string/split-lines
                     (slurp (.openStream (URL. "http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data")))))))

(first irisdata) ; ["5.1" "3.5" "1.4" "0.2" "Iris-setosa"]

(frequencies (map count irisdata)) ; {5 150} ; 150 data vectors

(defn parse-iris[i] (concat (map #(Double/parseDouble %) (take 4 i)) (drop 4 i)))

(map parse-iris irisdata)