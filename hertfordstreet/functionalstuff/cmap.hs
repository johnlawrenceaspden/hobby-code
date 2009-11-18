module CMap
    where

mymap f [] = []
mymap f (x:xs) = f x : mymap f xs

mysquare x = x*x

cpssquare x c = c (x*x)

cmap cpssquare [] = []
cmap cpssquare (x:xs) = cpssquare x (\a -> a:(cmap cpssquare xs))

tocps f = (\x g -> g(f x))

myfilter f [] = []
myfilter f (x:xs) = if (f x) then x:(myfilter f xs) else (myfilter f xs)

myeven 0 = True
myeven 1 = False
myeven n = not (myeven (n-1))

cpseven n c = c (myeven n)

cfilter cpseven [] = []
cfilter cpseven (x:xs) = cpseven x (\ b -> if b then (x:(cfilter cpseven xs)) else (cfilter cpseven xs))

main = do
  print (mymap (\x -> x*x) [1..10])
  print (cmap  cpssquare   [1..10])
  print (cmap  (tocps mysquare) [1..10])
  print (mymap myeven (mymap mysquare [1..100]))
  print (myfilter myeven (mymap mysquare [1..100]))
  print (cfilter cpseven (cmap cpssquare [1..100]))
  print (cfilter (tocps even) (cmap cpssquare [1..100]))