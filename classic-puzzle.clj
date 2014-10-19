;; A Classic Puzzle

;; This problem, my sources inform me, can be solved by pre-school
;; children in 5-10 minutes, by programmers - in 1 hour, and by
;; mathematicians ... well, check it yourself! :)

(def input (quote ("8809" "7111" "2172" "6666" "1111" "3213" "7662" "9313" "0000" "2222" "3333" "5555" "8193" "8096" "7777" "9999" "7756" "6855" "9881" "5531")))

(map classic input) ;-> (6 0 0 4 0 0 2 1 4 0 0 0 3 5 0 4 1 3 5 0)

;; What is:
(classic "2581")

;; What is classic ?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Just to make it a bit easier to read

(map (juxt identity classic) input) ;-> 

["8809" 6]
["7111" 0]
["2172" 0]
["6666" 4]
["1111" 0]
["3213" 0]
["7662" 2]
["9313" 1]
["0000" 4]
["2222" 0]
["3333" 0]
["5555" 0]
["8193" 3]
["8096" 5]
["7777" 0]
["9999" 4]
["7756" 1]
["6855" 3]
["9881" 5]
["5531" 0]



