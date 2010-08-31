;;The Sieve of Eratosthenes

;;We start with no primes, and a list of candidate integers.
#{} #{2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20}
;;The lowest of the candidates is a prime. 
;;Add it to the list of primes and remove all multiples of it from the candidates
#{2} #{3 5 7 9 11 13 15 17 19}
;;and repeat
#{2 3} #{5 7 11 13 17 19}
;;and repeat.
#{2 3 5} #{7 11 13 17 19}
;;Nothing interesting happens after here, because 5*5 is larger than 19, so we'd already have
;;eliminated anything that we're going to eliminate from now on, but let's not get too clever,
;;and follow the recursion to its natural end.
#{2 3 5 7} #{11 13 17 19}
#{2 3 5 7 11} #{13 17 19}
#{2 3 5 7 11 13} #{17 19}
#{2 3 5 7 11 13 17} #{19}
#{2 3 5 7 11 13 17 19} #{}
;;And we're done. No more candidates to sieve
#{2 3 5 7 11 13 17 19} #{}
#{2 3 5 7 11 13 17 19} #{}
;;so nothing happens at all from now on.

;;How to model this recursion with a function? 
;;We can use the clojure set library to do the striking off.
(use 'clojure.set)

;;Here's a function which takes one row from above, and produces the next:
(defn sievefn [[primes, candidates]]
  (if (empty? candidates) [primes, candidates]
      (let [prime (first candidates)             ;;the first candidate is always a prime
            end (inc (apply max candidates))     ;;we want to strike out all multiples
            multiples (range prime end prime)    ;;up to max(candidates)
            newprimes (conj primes prime)
            newcandidates (clojure.set/difference candidates multiples)]
        [ newprimes, newcandidates])))

;;Let's try it:
(def to20 [(sorted-set) #{2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20}])
(sievefn to20)
(sievefn (sievefn to20))

;;Here are the first ten iterations:
(take 10 (iterate sievefn to20))


;;Of course, this is all a bit long-winded. Let us instead define sieve like this,
;;mixing the iteration in with the transforming function:
(defn sieverecur [primes candset]
  (if (empty? candset) primes
      (let [prime (first candset)
            end (inc (apply max candset))]
        (recur (conj primes prime), (clojure.set/difference candset (range prime end prime))))))

(sieverecur (sorted-set) (apply sorted-set (range 2 100)))

;;We shouldn't have to recalculate the end value every time, though, so we can pull that bit out:
(defn sieverecur1 
  ([primes candset]
     (sieverecur1 primes candset (inc (apply max candset))))
  ([primes candset end]
     (if (empty? candset) primes
         (let [prime (first candset)]
           (recur (conj primes prime)
                  (clojure.set/difference candset (range prime end prime))
                  end)))))

(sieverecur1 (sorted-set) (apply sorted-set (range 2 100)))


;;Finally, we can incorporate our initial conditions, and also the optimization
;;that we noticed above, that nothing happens once the largest prime is above the
;;square root of the range.
(defn sieve
  ([n]
     (sieve (sorted-set) (apply sorted-set (range 2 (inc n))) (+ n 2)))
  ([primes candset end]
     (let [prime (first candset)]
       (if ( > (* prime prime) end)
         (clojure.set/union primes candset)
         (recur (conj primes prime)
                (clojure.set/difference candset (range prime end prime))
                end)))))

;;That's about it for the Sieve of Eratosthenes over a finite list.


;;What about producing an infinite list of primes?

;;A similar algorithm on paper might look like:

;;Start with a counter set to 2. Two is prime, so we can add it to a list of primes
;;The first multiple of 2 is 2, so we write [[multiple, prime]] counter
[[2,2]] 2                  
;;Now up the counter to 3. 
[[2,2]] 3
;;The multiple of 2 is lower than 3, so add 2 to get the next multiple of 2, 4
[[4,2]] 3
;;Now our multiple is higher, so we know 3 is also a prime, and we add it to the list
;;The first multiple of 3 is 3
[[3,3][4,2]] 3
;;Three is already on the list, so up the counter to 4.
[[3,3][4,2]] 4
;;4 is larger than our lowest prime multiple, so increase the multiple, by adding its prime.
[[4,2][6,3]] 4
;;We will keep the list of primes and multiples sorted in order of the multiples, so that we always
;;know which one to look at next.
;;We now have the primes 2 and 3, and their multiples 4 and 6
;;4 is equal to the lowest multiple. So discard it and increase the counter to 5
[[4,2][6,3]] 5
;;The lowest multiple is 4, less than 5, so we add its prime 2+4=6
[[6,2][6,3]] 5
;;5 is lower than any multiple, so it is a prime too.
;;And so on........
;;the iteration goes:
[[5,5][6,2][6,3]] 5
[[5,5][6,2][6,3]] 6
[[6,2][6,3][10,5]] 6
[[6,2][6,3][10,5]] 7
[[6,3][8,2][10,5]] 7
[[8,2][9,3][10,5]] 7
[[7,7][8,2][9,3][10,5]] 7
[[7,7][8,2][9,3][10,5]] 8
[[8,2][9,3][10,5][14,7]] 8
[[8,2][9,3][10,5][14,7]] 9
[[9,3][10,2][10,5][14,7]] 9
[[9,3][10,2][10,5][14,7]] 10
;;and as the counter increases, the primes accumulate on the left hand side.

;;We can construct a function which performs this iteration, too:
(defn infinite-sieve-fn [[testset int]]
  (let [pair (first testset)
        [multiple prime] pair]
    (cond (= int multiple) (list testset (inc int))
          (> int multiple) (list (conj (disj testset pair) [(+ multiple prime) prime]) int)
          (< int multiple) (list (conj testset [int int]) int))))

;;and iterate it infinitely:
(def sieves (iterate infinite-sieve-fn [(sorted-set [2,2]) 3]))
;;note the use of a sorted set, so that when we add in new pairs, the lowest will be the first element

;;Here are the first five iterations:
(take 5 sieves)

;;How to extract the primes?
;;Consider the 20th iteration
(nth sieves 20) 
;;is 
'(#{[10 2] [10 5] [12 3] [14 7]} 11)
;;So we'd like to extract the second elements of the first element
(map second (first (nth sieves 200)))
;;and it might be better if we sort them
(sort (map second (first (nth sieves 200))))

;;again we can construct an infinite list, derived from the first one
(def sieveprimes (map (fn[x] (sort (map second (first x)))) sieves))

;;What primes have we got after 1000 iterations?
(nth sieveprimes 1000)

;;after 10000 iterations, what are the last ten primes we found?
(take 10 (reverse (nth sieveprimes 10000)))

;;If all we want is a list of primes, it's silly to construct an entire list of iterations.
;;Just like above, we can fold the iteration into the function, but this time we need to 
;;decide when to stop iterating.
(defn infinite-sieve-recur [testset int stop]
  (if (> int stop) testset
      (let [pair (first testset)
            [multiple prime] pair]
        (cond (= int multiple) (recur testset (inc int) stop)
              (> int multiple) (recur (conj (disj testset pair) [(+ multiple prime) prime]) int stop)
              (< int multiple) (recur (conj testset [int int]) int stop)))))

;;here's the test set when the counter has got to ten.
(infinite-sieve-recur (sorted-set [2,2]) 2 10)

;;now we can ask for all the primes up to 100.
(map second (infinite-sieve-recur (sorted-set [2,2]) 2 100))

;;we've lost memoization by abandoning the infinite sequence, but we also don't need to
;;keep all that intermediate data in memory for ever.
;;This alone has given us a speed up of a factor of 100.

;;Here are the last ten primes before 10000
(take 10 (reverse (sort (map second (infinite-sieve-recur (sorted-set [2,2]) 2 10000)))))

;;Again, there's no point in testing numbers for factors over their square root, so we can optimise that 
;;by setting the first test multiple of a prime to be its square. 
;;Note that we now need to remember to up the counter at the same time!
[[4,2]] 2
[[4,2]] 3
[[4,2][9,3]] 4
[[4,2][9,3]] 5
[[6,2][9,3][25,5]] 5
[[6,2][9,3][25,5]] 6
[[8,2][9,3][25,5]] 7
[[8,2][9,3][25,5][49,7]] 8
[[8,2][9,3][25,5][49,7]] 9
[[9,3][10,2][25,5][49,7]] 9
[[9,3][10,2][25,5][49,7]] 10
[[9,3][10,2][25,5][49,7]] 11

;;And again, we may as well fold in our initial conditions to make a tidy function
(defn infinite-sieve 
  ([n] (sort (map second (infinite-sieve (sorted-set [2,2]) 2 n))))
  ([testset int stop]
  (if (> int stop) testset
      (let [pair (first testset)
            [multiple prime] pair]
        (cond (= int multiple) (recur testset (inc int) stop)
              (> int multiple) (recur (conj (disj testset pair) [(+ multiple prime) prime]) int stop)
              (< int multiple) (recur (conj testset [(* int int) int]) (inc int) stop))))))

(infinite-sieve 110)
(take 10 (reverse (infinite-sieve 10000)))


;;Now all we have to do is figure out why the thing is so slow!
;;Which I think will be a blog post for another day.

;;--------------------------------------------------------------------------------------------


;;(time (take 10 (sieve (sorted-set [4,2]) 2 1000000))) 60312 msecs

;;This is still taking about 60 seconds to find the 78000 primes up to 1000000

;;--------------------------------------------------------------------------------------
;;This is still taking about a second on my little netbook, which is worryingly slow 
;;for 30000 or so iterations of a simple thing.
;;The most complicated iteration is only to replace a pair in a sorted set with another pair, found by addition. We're using about 30 microseconds per iteration.
;;I'm guessing that that means about 30000 machine instructions for a single iteration.

;;Now we need to start worrying about optimization. Here are some ideas:

;;I think that there's a further algorithmic speedup, from using a priority queue rather than a sorted list.
;;After that we probably need to add type hints to avoid dynamic dispatch.
;;Then we need to start using the underlying java integers instead of clojure's arbitrary precision
;;arithmetic. Of course, that will break the program.
;;If we're going to do that then we might as well find a nice priority queue library and code it up in C!

;;Luckily I am very tired, and want to go to bed now.....

;;(time (take 10 (sieverecur1 (sorted-set) (apply sorted-set (range 2 1000000))))) 25040ms

;;(time (take 10 (sieve 1000000))) 21748ms


;;On my desktop, that takes 20 seconds to do all the primes up to 1000000 
;;That's about 20 000 000 000 operations
;;more generally, the time looks a bit like n log n

(time (take 10 (reverse (nth sieveprimes 10000))))

;;On my desktop, that takes about 4 seconds.
;;All I have to do now is work out why the thing is so beastly slow!
;;This is how long it takes to calculate up to 1000000:
;;(time (take 10 (map second (sieve (sorted-set [2,2]) 2 1000000))))

;;It's now taking 72 seconds to calculate up to 1000000. It's not clear to me why it's slower than the finite version.
;;It may be because constructing a list to delete, and then using set/difference is better than deleting individual elements.

