(use '[clojure.contrib.str-utils :only (str-join)])

(defn pig-latin [word]
  (let [first-letter (first word)]
    (if ((set "aeiou") first-letter)
      (str word "ay")
      (str (subs word 1) first-letter "ay"))))


(defn pls [sentence]
  (str-join " "
      (map pig-latin
        (re-seq #"\w+" sentence))))

(pls "red orange yellow green blue indigo violet")

(pls "The quality of mercy is not strained. It falleth as the gentle rain from heaven.")

