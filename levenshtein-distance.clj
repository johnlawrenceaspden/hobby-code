;; The other day I needed a way of measuring the distance between two strings

;; Levenshtein distance is one such way:

;; avada kedavra
;; abada kedavra
;; abrada kedavra
;; abrada cedavra
;; abrada cadavra
;; abrada cadabra
;; abraa cadabra
;; abra cadabra
;; abracadabra

;; Eight changes get you from one to the other

;; Thank you google, wikipedia and Jeff Foster's excellent blog fatvat (http://www.fatvat.co.uk/)

(defn levenshtein-distance
  "Calculates the edit-distance between two sequences"
  [seq1 seq2]
  (cond
   (empty? seq1) (count seq2)
   (empty? seq2) (count seq1)
   :else (min
          (+ (if (= (first seq1) (first seq2)) 0 1)
             (#'levenshtein-distance (rest seq1) (rest seq2))) 
          (inc (#'levenshtein-distance (rest seq1) seq2))      
          (inc (#'levenshtein-distance seq1 (rest seq2))))))

(levenshtein-distance "avada kedavra" "abracadabra") ;; much grinding......

;; We cast a spell more powerful than either:
(def levenshtein-distance (memoize levenshtein-distance))

(levenshtein-distance "avada kedavra" "abracadabra") ; ok, it was seven.

