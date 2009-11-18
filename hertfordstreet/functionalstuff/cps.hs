module CPS
    where

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

cfold' f z [] = z
cfold' f z (x:xs) = f x z (\y -> cfold' f y xs)

cfold f z l = cfold' (\x t g -> f x (g t)) z l


main = do 
  print "hello"
  print (cfold (+) 0 [1,2,3,4])
  print (cfold (:) [] [1,2,3])
  print (cfold' (\x t g -> (x : g t)) [] [1..10])
  print (cfold' (\x t g -> g (x:t)) [] [1..10])


evaluationorder = do
  print ([ (cfold (+) 0 [1,2,3,4]),
           (cfold' (\x t g -> (+) x (g t)) 0 [1,2,3,4]),
           (cfold (+) 0 []),
           (cfold' (\x t g -> (+) x (g t)) 0 [])
         ])

--the identity function in CPS
myid :: a->a
myid a = a

idCPS :: a -> (a -> r) -> r
idCPS a ret = ret a

doid = do
  print (myid 2)
  idCPS 2 print

--the square root function in CPS
mysqrt :: Floating a => a -> a
mysqrt a = sqrt a

mysqrtCPS ::  (Floating a) => a -> (a -> r) -> r
mysqrtCPS a k = k (sqrt a)

dosqrt = do
  print (mysqrt 4)
  mysqrtCPS 4 print

--the factorial function in CPS
fac :: Integral a => a -> a
fac 0 = 1
fac n = n * fac (n-1)

facCPS 0 k = k 1
facCPS n k = facCPS (n-1) (\x -> k ( n * x ))

dofac = do
  print (fac 10)
  facCPS 10 print
























