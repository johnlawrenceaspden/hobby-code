;; The Unexpected Appearance of Schlemiel, the Painter

;; So, it is clear that my model of how things are done was badly broken

;; I am doing some statistics, one day, and so I define:

;; the average of a finite sequence
(defn average [sq] (/ (reduce + sq) (count sq)))

;; and the square of a number
(defn square [x] (* x x))

;; and a way of forgetting about all the fiddly little digits at the end
(defn twosf   [x]  (float (/ (Math/round (* x 100.0)) 100))) 

;; but for the variance I am a little torn between:
(defn variance-one [sq]
  (let [av (average sq)]
    (average (map #(square (- % av)) sq))))

;; ;

(defn variance-two [sq]
  (let [sqdiff #(square (- % (average sq)))]
    (average (map  sqdiff sq))))

;; and (I have a regrettable weakness for the terse...) 
(defn variance-one-liner [sq] (average (map #(square (- % (average sq))) sq)))

;; but what I am not expecting, is this: 

(let [s (repeatedly 1000 #(rand))]
  (twosf (reduce + s)) ;; just to force the sequence to be generated before timing things
  [(time (twosf (reduce + s)))
   (time (twosf (average  s)))
   (time (twosf (variance-one s)))
   (time (twosf (variance-two s)))
   (time (twosf (variance-one-liner s)))])

;; "Elapsed time: 0.535715 msecs"
;; "Elapsed time: 0.834523 msecs"
;; "Elapsed time: 1.417108 msecs"
;; "Elapsed time: 251.650722 msecs"
;; "Elapsed time: 248.196331 msecs"
;; [496.83 0.5 0.09 0.09 0.09]


;; It seems that all these functions are correct, in the sense that they are producing correct-looking answers, and yet:

;; It seems that variance-one is doing what I expect, running down the sequence twice and ending up taking about twice as long as averaging it.

;; But that the other two are taking hundreds of times longer, possibly because they are re-calculating the average of the sequence every time.

;; I had a nice hour or so, thinking about what was going on here, and why, and wonder if you might enjoy the same thoughts, dear readers.
