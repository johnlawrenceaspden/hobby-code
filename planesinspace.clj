;; Into how many regions can six planes divide space?

;; Too hard!

;; So make it harder:

;; Into how many regions can m hyperplanes divide n dimensional space?

;; Now look at the easiest case.

;; Into how many lines can m points divide a line?

;; We start off with one line and no points

;; n = 1 
;; m  points lines
;; 0  0      1

;; Every time we add a point, it splits a line

;; n = 1 
;; m  points lines
;; 0  0      1
;; 1  1      2
;; 1  1      2

(defn regions [n m]
  (cond (= n 1) (list m (inc m))
        :else 'chsaispas))

(regions 1 10) ;-> (10 11)
;; Adding ten points turns a line into 11 lines and 10 points.

;; Ok, what about lines dividing a plane?

;; We start off with one area, no lines and no points.

;; n = 2 
;; m  points lines areas
;; 0  0      0     1

;; When we add a line, it splits the area into two areas, so we have
;; no points, one line and two areas

;; n = 2 
;; m  points lines areas
;; 0  0      0     1
;; 1  0      1     2

;; When we add a second line, unless we've accidentally put it
;; parallel to our first line, in which case we aren't doing as well
;; as we could do, it will cross the first line.

;; So our new line will be actually be one point and two lines, 
;; and the one point will divide the first line in two, 
;; and the two lines will cut the two areas into four.

;; 0      1     2  ;; State with one line
;; 1      2        ;; Added one point and two lines
;;        1     2  ;; Which have split one line and two areas
;; 1      4     4  ;; So two lines divide a plane into one point, four lines and four areas

;; n = 2 
;; m  points lines areas
;; 0  0      0     1
;; 1  0      1     2
;; 2  1      4     4

;; When we add a third line, it will cross both previous lines, and so it will be 
;; a line divided by two points 

(regions 1 2) ;-> (2 3)

(map + '(1 4 4) 
     (concat '(0) (regions 1 2)) 
     (concat (regions 1 2) '(0))) ;-> (3 9 7)

;; If you draw the figure, you'll see that it indeed has a triangle with three vertices, 
;; which split the three lines into 9, and there are seven distinct regions.

;; So, taking a blind leap in the dark...

(defn regions [n m]
  (cond (= n 1) (list m (inc m))
        (= m 0) (concat (repeat n 0) '(1))
        :else (map + 
                   (regions n (dec m))
                   (concat '(0) (regions (dec n) (dec m))) 
                   (concat (regions (dec n) (dec m)) '(0)))))

(regions 1 0) ;-> (0 1)
(regions 1 1) ;-> (1 2)
(regions 1 2) ;-> (2 3)
(regions 1 3) ;-> (3 4)

(regions 2 0) ;-> (0 0 1)
(regions 2 1) ;-> (0 1 2)
(regions 2 2) ;-> (1 4 4)
(regions 2 3) ;-> (3 9 7)
(regions 2 4) ;-> (6 16 11)
(regions 2 5) ;-> (10 25 16)

(regions 3 1) ;-> (0 0 1 2)
(regions 3 2) ;-> (0 1 4 4)
(regions 3 3) ;-> (1 6 12 8)
(regions 3 4) ;-> (4 18 28 15)
(regions 3 5) ;-> (10 40 55 26)
(regions 3 6) ;-> (20 75 96 42)

;; Into how many regions can six planes divide space?

(last (regions 3 6)) ;-> 42



