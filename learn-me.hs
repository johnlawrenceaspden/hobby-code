


7+8

succ 7 * 8 + 1
succ (7 * (8 + 1))

92/10
92 `div` 10
div 92 10
(* 9 (div 92 10))

:l baby

doubleUs 23 4 + doubleMe 7


let doubleMe x = x + x + x

doubleMe 7

let l=[1,2,3,4]
let r=[5,6,7,8]
map doubleMe l ++ r
map doubleMe (l ++ r)
let whole= 0:l++r
whole
map doubleMe whole

'A':' ':"small"++" cat"
1:2:3:[]
1:2:3:[] !! 1
(1:2:3:[]) !! 1

[3,2,1] > [1,2,3]

head [1,2,3,4]

tail [1,2,3,4]

last [1,2,3,4]

init [1,2,3,4]

head []

let reduceplus l = (head l)+(reduceplus (tail l))

reduceplus [1,2,3,4]
let reduceplus l = if (null l) then 0 else (head l)+(reduceplus (tail l))
reduceplus [1,2,3,4]

let reduceplus l = if (null l) then 0 else ((head l)+(reduceplus (tail l)))
reduceplus [1,2,3,4]

reverse [1,2,3,4]

take 3  [1,2,3,4]

drop 3  [1,2,3,4]

maximum  [1,2,3,4]

minimum  [1,2,3,4]

minimum  (reverse [1,2,3,4])

sum [1,2,3,4]
product [1,2,3,4]

(elem 3 [1,2,3,4])

3 `elem` [1,2,3,4]

product [1..2000]

['a'..'z']
['K'..'Z']

[1,3..20]
['a',3..'z']

[20,-1..19]
[20,19..1]

maximum ([0.1,0.3..1.0])

product (take 24 [1..])

(reduce * (range 1 25))

take 12 [13,26..]

let square x = x*x

take 20 (map square [1..])

take 20 (cycle [1,2,3])






















