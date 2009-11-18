module CFold
    where

--fold function in continuation passing style

cfold' f z [] = z                --fold empty list gives zero
--fold of populated list gives 'combine head with zero and pass to cfold as new zero
cfold' f z (x:xs) = f x z (\y -> cfold' f y xs)

cfold f = cfold' (\x t g -> f x (g t)) 

numlist = [1..10]

sumlist  =  cfold (+) 0

prodlist  = cfold (*) 1

idlist  = cfold (:) [] 

lenlist  = cfold (\ x s -> s+1) 0 

maxlist l = cfold max (head l) (tail l)
minlist l = cfold min (head l) (tail l)

main = do
  print (map (\f -> f numlist) [sumlist, prodlist, maxlist, minlist, lenlist])
  print (idlist numlist)
  print (workinglist)
  print (cfold' (\x t g -> g (x : t)) [] [1..10])
  print (cfold' (\x t g -> x : (g t)) [] [1..10])

workinglist = 
    [ 
     cfold (+) 0 [1]  --fold in sane notation
    ,
     let f = (+) in
     let z = 0 in
     let l = [1] in
     cfold' (\x t g -> f x (g t)) z l 
    ,
     cfold' (\x t g -> (+) x (g t)) 0 [1] --fold now in CPS
    ,
     cfold' (\x t g -> (+) x (g t)) 0 [1]
    ,
     let f = (\x t g -> (+) x (g t)) in
     let z = 0 in
     let x = 1 in
     let xs = [] in
     f x z (\y -> cfold' f y xs)
    ,

     (\x t g -> (+) x (g t)) 1 0 (\y -> cfold' (\x t g -> (+) x (g t))  y [])
    ,
     let x = 1 in
     let t = 0 in
     let g = (\y -> cfold' (\x t g -> (+) x (g t))  y []) in
     (+) x (g t)
    ,
     (+) 1 ((\y -> cfold' (\x t g -> (+) x (g t))  y []) 0)
    ,
     (+) 1 (let y = 0 in
            (cfold' (\x t g -> (+) x (g t)) y []))
    ,
     (+) 1 (cfold' (\x t g -> (+) x (g t)) 0 [])
    ,
     (+) 1 0
    ,
     1
    
    ]

   
folds=([     1-(2-(3-(4-(5-0)))),
     (cfold (-) 0 [1,2,3,4,5]),
     (foldr (-) 0 [1,2,3,4,5]),
     (foldl (-) 0 [1,2,3,4,5])],[
     (foldr (\a b -> "("++(show a)++"-"++b++")") (show 0) [1..5]),
     (cfold (\a b -> "("++(show a)++"-"++b++")") (show 0) [1..5]) ])

threefolds z l =
    let combine a b = "("++a++"~"++b++")" in
    let zero = show z in 
    map (\f -> f combine zero (map show l)) [foldr, cfold, foldl]
                                                            
manyfolds = map (threefolds 0) (map (\n -> [1..n]) [0..3])
