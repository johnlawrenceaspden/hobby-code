;(use 'clojure.contrib.error-kit)

(require '(clojure.contrib [error-kit :as kit]))

(kit/deferror *number-error* [] [n]
  {:msg (str "Number error: " n)
   :unhandled (kit/throw-msg NumberFormatException)})

(kit/deferror *odd-number-error* [*number-error*]
  "Indicates an odd number was given to an operation that is only
    defined for even numbers."
  [n]
  {:msg (str "Can't handle odd number: " n)}) 

(defn int-half [i]
  (if (even? i)
    (quot i 2)
    (kit/raise *odd-number-error* i)))

(int-half 4)
(int-half 3)

(vec (map int-half [2 3 4 5]))
(map int-half [2 3 4])

(kit/with-handler
  (map int-half [2 4 5 8])
  (kit/handle *odd-number-error* [n]
              (throw (Exception. (format "Odd number %d in vector." n)))))

;;This is synonymous with:
(kit/with-handler
  (vec (map int-half [2 4 5 8]))
  (kit/handle {:keys [n tag]}
              (if (isa? tag `*odd-number-error*)
                (throw (Exception. (format "Odd number %d in vector." n)))
                (kit/do-not-handle))))

(kit/with-handler
  (vec (map int-half [2 4 5 8]))
  (kit/handle *odd-number-error* [n]
              "yo"))

(kit/with-handler
  (vec (map int-half [2 4 5 8]))
  (kit/handle *odd-number-error* [n]
              (kit/continue-with 0)))

(kit/with-handler
  (vec (map int-half [2 4 5 8]))
  (kit/handle *odd-number-error* [n]
              (kit/continue-with (int-half (+ n 1)))))

(kit/with-handler
  (vec (map int-half [2 4 5 8]))
  (kit/handle *odd-number-error* [n]
              (kit/continue-with (int-half (- n 1)))))

;;doesn't play terribly well with laziness. 
(kit/with-handler
  (map int-half [2 4 5 8])
  (kit/handle *odd-number-error* [n]
              (kit/continue-with 0)))


(defn int-half-vec [s]
  (reduce (fn [v i]
            (kit/with-handler
              (conj v (int-half i))
              (kit/bind-continue instead-of-half [& instead-seq]
                                 (apply conj v instead-seq))))
          [] s))

(kit/with-handler
  (int-half-vec [2 4 5 8])
  (kit/handle *number-error* [n]
              (kit/continue instead-of-half :oops n))) 

