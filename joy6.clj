(defprotocol Concatenable
  (cat [this other]))

(extend-type String
  Concatenable
  (cat [this other]
       (.concat this other)))

(extend-type java.util.List
  Concatenable
  (cat [this other]
       (concat this other)))

(cat "House" " of Leaves") ; "House of Leaves"
(cat '(1 2 3) '(:a :b :c)) ; (1 2 3 :a :b :c)
(cat '(1 2 3) " of Leaves") ; (1 2 3 \space \o \f \space \L \e \a \v \e \s)
(cat " of Leaves" '(1 2 3)) ; error

;; So defprotocol/extend type are java interfaces and single argument dispatch?
;; Except that we can extend a protocol to a pre-existing type, it doesn't have to implement the interface.


(ns joy.chess)

(defn initial-board []
  [\r \n \b \q \k \b \n \r  
   \p \p \p \p \p \p \p \p  
   \- \- \- \- \- \- \- \­  
   \- \- \- \- \- \- \- \­  
   \- \- \- \- \- \- \- \­  
   \- \- \- \- \- \- \- \­  
   \P \P \P \P \P \P \P \P  
   \R \N \B \Q \K \B \N \R])

(initial-board)

(letfn [(index [file rank]
               (let [f (- (int file) (int \a))
                     r (* 8 (- 8 (- (int rank) (int \0))))]
                 (+ f r)))]
  (defn lookup [board pos]
    (let [[file rank] pos]
      (board (index file rank)))))

(lookup (initial-board) "e1") ; \K


(defn lookup2 [board pos]
  (let [[file rank] (map int pos)
        [fc rc]     (map int [\a \0])
        f (- file fc)
        r (* 8 (- 8 (- rank rc)))
        index (+ f r)]
    (board index)))

(lookup (initial-board) "e8") ; \k











