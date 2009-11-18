;;sequence xor from http://bc.tech.coop/blog/081029.html
(defn seq-xor [& seqs]
  "find all the things which are only present in one of the sequences
   (seq-xor \"Hello\" \"World\") -> (\\d \\e \\H \\r \\W)"
  (seq (second
	(reduce (fn [[all ret] x]
		  (if (contains? all x)
		    [all (disj ret x)]
		    [(conj all x) (conj ret x)]))
		[#{} #{}] (mapcat distinct seqs)))))

;;test cases
(and
 (= (seq-xor [\a \b \c \b \a] [\a \g \g \f]) '(\b \c \f \g))
 (= (seq-xor "Hello" "World") '(\d \e \H \r \W))
 (= (seq-xor (range 10 20) (range 5 15) (range 0 5)) '(0 1 2 3 4 5 6 7 8 9 15 16 17 18 19)))

;;how it works

;;first mapcat & distinct collapse multiple entries in the same sequences
(mapcat distinct [[\a \b \c \a \b] [\a \g \f]])
(\a \b \c \a \g \f)


;;then a clever reduction on the sequence, using two sets as accumulators
;;all holds everything we've seen up to now
;;ret holds everything we've only seen once so far
;;for every new element, if it's not been seen, add it to both sets, 
;;and if it's already been seen, knock it out of ret.
(defn reducing-fn [[all ret] x]
		    (if (contains? all x)
		      [all (disj ret x)]
		      [(conj all x) (conj ret x)]))


;;here's an example reduction step by step
(reducing-fn [#{} #{}]  \a )
(reducing-fn [#{\a} #{\a}]  \b )
(reducing-fn [#{\a \b} #{\a \b}] \c)
(reducing-fn [#{\a \b \c} #{\a \b \c}] \a)
(reducing-fn [#{\a \b \c} #{\b \c}] \g)
(reducing-fn [#{\a \b \c \g} #{\b \c \g}] \f)

;;giving the final result:
[#{\a \b \c \f \g} #{\b \c \f \g}]





