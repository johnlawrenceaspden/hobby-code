;; The Triumph of the Median

;; http://lesswrong.com/lw/isk/a_voting_puzzle_some_political_science_and_a_nerd/
;; And originally from Sideways Stories from Wayside School by Louis Sachar

;; The students have Mrs. Jewl's class have been given the privilege
;; of voting on the height of the school's new flagpole. She has each
;; of them write down what they think would be the best height for the
;; flagpole. The votes are distributed as follows:

;;     1 student votes for 6 feet.
;;     1 student votes for 10 feet.
;;     7 students vote for 25 feet.
;;     1 student votes for 30 feet.
;;     2 students vote for 50 feet.
;;     2 students vote for 60 feet.
;;     1 student votes for 65 feet.
;;     3 students vote for 75 feet.
;;     1 student votes for 80 feet, 6 inches.
;;     4 students vote for 85 feet.
;;     1 student votes for 91 feet.
;;     5 students vote for 100 feet.

;; At first, Mrs. Jewls declares 25 feet the winning answer, but one
;; of the students who voted for 100 feet convinces her there should
;; be a runoff between 25 feet and 100 feet. In the runoff, each
;; student votes for the height closest to their original answer. But
;; after that round of voting, one of the students who voted for 85
;; feet wants their turn, so 85 feet goes up against the winner of the
;; previous round of voting, and the students vote the same way, with
;; each student voting for the height closest to their original
;; answer. Then the same thing happens again with the 50 foot
;; option. And so on, with each number, again and again, "very much
;; like a game of tether ball."

;; Question: if this process continues until it settles on an answer
;; that can't be beaten by any other answer, how tall will the new
;; flagpole be?

;; The most interesting part of this problem is to work out an incantation in emacs
;; that can turn

;;     1 student votes for 6 feet.
;;     1 student votes for 10 feet.
;;     7 students vote for 25 feet.
;;     1 student votes for 30 feet.
;;     2 students vote for 50 feet.
;;     2 students vote for 60 feet.
;;     1 student votes for 65 feet.
;;     3 students vote for 75 feet.
;;     1 student votes for 80 feet, 6 inches.
;;     4 students vote for 85 feet.
;;     1 student votes for 91 feet.
;;     5 students vote for 100 feet.

;; into

(def votes (concat
            (repeat 1,6)
            (repeat 1,10)
            (repeat 7,25)
            (repeat 1,30)
            (repeat 2,50)
            (repeat 2,60)
            (repeat 1,65)
            (repeat 3,75)
            (repeat 1,80.5)
            (repeat 4,85)
            (repeat 1,91)
            (repeat 5,100)))

;; which I leave as an exercise to the reader.

;; the lucky winner is 25 feet, with seven votes, and 100 feet getting 5 votes
(reverse (sort-by second (frequencies votes))) ;-> ([25 7] [100 5] [85 4] [75 3] [60 2] [50 2] [30 1] [91 1] [10 1] [6 1] [65 1] [80.5 1])

;; how do we do a run off between two options?

;; firstly let's ask whether a single voter would prefer option 1
(defn closer [vote option1 option2]
  (< (Math/abs (- option1 vote))
     (Math/abs (- option2 vote))))

(closer 26 25 100) ;-> true (one who likes 26 prefers 25 to 100)
(closer 50 25 100) ;-> true 
(closer 75 25 100) ;-> false (one who prefers 75 prefers 100 over 25)

;; given this choice between 100 and 25, how many prefer 25?
(frequencies (map (fn[x] (closer x 25 100)) votes)) ;-> {true 14, false 15}



;; So 100 is (just) stable against challenges from 25

;; But although 100 survived the challenge from 25, it still has to face 
;; a second challenge from partisans of 85

(defn winner [incumbent challenger votes]
  (let [v (frequencies (map (fn[x] (closer x incumbent challenger)) votes))]
    (if (< (v false 0) (v true 0))
      incumbent
      challenger)))


(winner 25 100 votes) ;-> 100
(winner 85 100 votes) ;-> 85

;; Now 50 gets its turn....

(winner 50 100 votes) ;-> 50

;; as do all the other attempts
(distinct votes) ;-> (6 10 25 30 50 60 65 75 80.5 85 91 100)

;; fifty can survive a lot of challenges but eventually 65 can beat it.
(reductions (fn [a b] (winner a b votes)) 50 (distinct votes))
 ;-> (50 50 50 50 50 50 60 65 65 65 65 65 65)

;; but 65 can survive any challenge
(reductions (fn [a b] (winner a b votes)) 65 (distinct votes))
 ;-> (65 65 65 65 65 65 65 65 65 65 65 65 65)

;; But it's not at all clear that this outcome is inevitable

(reduce (fn [a b] (winner a b votes)) (rand-nth votes) (shuffle votes)) ;-> 65

;; Careful examination of various possible run-offs shows that 
(for [a (distinct votes)]
  (for [b (distinct votes)]
  (winner a b votes)))

((6 10 25 30 50 60 65 75 80.5 85 91 100) 
 (10 10 25 30 50 60 65 75 80.5 85 91 100) 
 (25 25 25 30 50 60 65 75 80.5 85 91 100) 
 (30 30 30 30 50 60 65 75 80.5 85 91 100) 
 (50 50 50 50 50 60 65 75 50 50 50 50) 
 (60 60 60 60 60 60 65 60 60 60 60 60) 
 (65 65 65 65 65 65 65 65 65 65 65 65) 
 (75 75 75 75 75 60 65 75 75 75 75 75) 
 (80.5 80.5 80.5 80.5 50 60 65 75 80.5 80.5 80.5 80.5) 
 (85 85 85 85 50 60 65 75 80.5 85 85 85) 
 (91 91 91 91 50 60 65 75 80.5 85 91 91) 
 (100 100 100 30 50 60 65 75 80.5 85 91 100))


(count votes) ;-> 29

;; There's an easier way to get this answer: 
(def m (quot 29 2))
(nth (sort votes) m)

;; The votes will settle on the median. By symmetry I imagine that if
;; there were an even number of people there might be two stable
;; answers, and whichever got picked first would win forever.

;; This sort-of-explains why:

;; (a) Two ice-cream stalls will set up right next to each other in
;; the middle of a beach, rather than far apart

;; (b) Political parties in a democracy end up having very similar
;; views, which means that voters get no effective choice in an
;; election, whilst nevertheless getting a government that is
;; reasonably in tune with the popular mood.











