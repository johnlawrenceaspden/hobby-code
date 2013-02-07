;; List Comprehension vs Map and Filter

;; The consequences of map taking many possible arguments were not clear to me until I realized that
;; it's 'side by side' iteration.

;; I asked this question:
;; http://stackoverflow.com/questions/14737391/how-do-i-combine-two-vectors-of-vectors-element-wise-in-clojure
;; on Stack Overflow, and the answer made me feel like a complete idiot.

;; ---------------------------------------------------------------------------------------

;; I can write:

(for [i [1 2 3]] (* 2 i)) ; (2 4 6)

;; Or I can write 

(map (fn[i] (* 2 i)) [1 2 3]) ; (2 4 6)

;; And which is easier to read is a bit of a matter of taste

;; I can write 

(for [i [1 2 3]] (for  [j [4 5 6]] (* i j))) ; ((4 5 6) (8 10 12) (12 15 18))

;; Or I can write 

(map (fn[j] (map (fn[i] (* i j)) [4 5 6])) [1 2 3]) ; ((4 5 6) (8 10 12) (12 15 18))

;; And I think most people will find the first one easier to read. Certainly I found it easier to write.

;; I can write 
(for [i [1 2 3], j [4 5 6]] (* i j)) ; (4 5 6 8 10 12 12 15 18)

;; Or I can write 
(mapcat (fn[j] (map (fn[i] (* i j)) [4 5 6])) [1 2 3]) ; (4 5 6 8 10 12 12 15 18)

;; And so I had a sort of feeling that list comprehension and map/filter were equivalent but that
;; list comprehensions were somewhat easier to read and write.

;; This certainly seems to be the case in python, where there have been occasional attempts to take
;; map, filter and reduce out of the language, in favour of list comprehensions


;; But occasionally, I'd like to iterate over two structures at once.

;; So [1 2 3], [4 5 6], * -> [4 10 18], say

;; And I am usually writing some horror like:

(for [[i,j] (partition 2 (interleave [1 2 3] [4 5 6]))] (* i j)) ; (4 10 18)

;; But in fact:

(map * [1 2 3] [4 5 6]) ; (4 10 18)

;; In which case I am thinking that the version with map is way clearer, and I am also thinking that
;; perhaps list comprehensions cannot do everything that map and filter can.

;; Or maybe I just don't know the syntax for side by side iteration. Or even the proper name. The
;; earlier combination is called 'cartesian product', and I am tempted to call the latter one
;; 'elementwise combination'.

;; Can anyone enlighten me on this subject?

;; Here's a nice application of multi-argument map, closely related to a real problem I'm trying to solve:

(def a '[[c c c]
         [y y y]
         [m m m]])

(def b '[[r g b]
         [r g b]
         [r g b]])

(def c '[[x y z]
         [y z x]
         [z x y]])

(mapv (partial mapv vector) a b c) 
;  [[[c r x] [c g y] [c b z]] 
;   [[y r y] [y g z] [y b x]] 
;   [[m r z] [m g x] [m b y]]]

;; Thank you to Alex Stoddard for that, and indeed a certain amount of associated enlightenment!

































(mapv + [1 2 3] [4 5 6])

(mapv vector [1 2 3] [4 5 6])




(mapv (partial mapv vector) a b c)

; [[[c r] [c g] [c b]] [[y r] [y g] [y b]] [[m r] [m g] [m b]]]