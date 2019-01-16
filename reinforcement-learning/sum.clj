
(float (+ 1)) ; 1.0
(float (+ 1 1/4)) ; 1.25
(float (+ 1 1/4 1/16)) ; 1.3125
(float (+ 1 1/4 1/16 1/64)) ; 1.328125
(float (+ 1 1/4 1/16 1/64 1/256)) ; 1.3320312
(float (+ 1 1/4 1/16 1/64 1/256 1/1024)) ; 1.3330078

(take 7 (reductions + (iterate #(/ % 4) 1))) ; (1 5/4 21/16 85/64 341/256 1365/1024 5461/4096)

(float (+ 5 4/2 5/4 4/8 5/16 4/32 5/64 4/128 5/256 4/512 5/1024)) ; 9.329102

(map float
     (reductions +
                 (map * 
                      (apply concat (repeat [5 4]))
                      (iterate #(/ % 2) 1))))

(map float
     (reductions +
                 (map * 
                      (apply concat (repeat [4 5]))
                      (iterate #(/ % 2) 1))))
