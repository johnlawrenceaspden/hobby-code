module Count 
    where

import Char

count1 p l = length (filter p l)

count2 p l = foldr (\x c -> if p x then c+1 else c) 0 l

data Pair a b = Pair a b
pairFst (Pair x y) = x
pairSnd (Pair x y) = y

data Triple a b c = Triple a b c
tI   (Triple x y z) = x
tII  (Triple x y z) = y
tIII (Triple x y z) = z

split a = tI a: tII a: tIII a: []

data Quadruple a b = Quadruple a a b b
firstTwo (Quadruple x y z w) = [x, y]
lastTwo (Quadruple x y z w) = [z, w]


data MyMaybe a = MyNothing | MyJust a

firstElement :: [a] -> Maybe a
firstElement [] = Nothing
firstElement (x:xs) = Just x

findElement :: (a->Bool) -> [a] -> Maybe a
findElement p [] = Nothing
findElement p (x:xs) = 
    if p x then Just x
    else findElement p xs


data Tuple a b c d = T4 a b c d | T3 a b c | T2 a b | T1 a
tuple1 (T4 x y z w) = Just x
tuple1 (T3 x y z ) = Just x
tuple1 (T2 x y) = Just x
tuple1 (T1 x ) = Just x
tuple2 (T4 x y z w) = Just y
tuple2 (T3 x y z ) = Just y
tuple2 (T2 x y) = Just y
tuple2 (T1 x ) = Nothing
tuple3 (T4 x y z w) = Just z
tuple3 (T3 x y z ) = Just z
tuple3 (T2 x y) = Nothing
tuple3 (T1 x ) = Nothing
tuple4 (T4 x y z w) = Just w
tuple4 (T3 x y z ) = Nothing
tuple4 (T2 x y) = Nothing
tuple4 (T1 x ) = Nothing

tuplize (T1 x) = Left (Left x)
tuplize (T2 x y) = Left (Right (x,y))
tuplize (T3 x y z) = Right (Left (x,y,z))
tuplize (T4 x y z w) = Right (Right (x,y,z,w))

--completely spurious, but neat!
pythagorean_triples n =
    [ (x, y, z) | x <-[1..n], y<-[x..n], z<-[y..n], x^2 + y^2 == z^2 ]

data MyList a = Nil
              | MyCons a (MyList a)

listLength Nil = 0
listLength (MyCons x xs) = 1 + listLength xs
listHead (MyCons x xs) = x
listTail (MyCons x xs) = xs

samplelist = (MyCons "fluffy" (MyCons "doom" (MyCons "2" Nil)))
numlist = (MyCons 10 (MyCons 3 (MyCons 7 Nil)))

--(fl op zero '(a b c)) -> (op (op (op zero a) b) c) = (((zero `op` a) `op` b) `op` c)
myListFoldl op zero Nil = zero
myListFoldl op zero (MyCons x xs) = (myListFoldl op (op zero x) xs)

--(fr op zero '(a b c)) -> (op a (op b (op c zero))) = (a `op` (b `op` (c `op` zero)))
myListFoldr op zero Nil = zero
myListFoldr op zero (MyCons x xs) = (op x (myListFoldr op zero xs))



data BinaryTree a
    = Leaf a
      | Branch (BinaryTree a) a (BinaryTree a)

listtotree [a,b,c,d,e] = Branch (Leaf a) b (Branch (Leaf c) d (Leaf e))
stringify (Branch x y z) = "(" ++ (stringify x) ++ "," ++ (show y) ++ "," ++ (stringify z) ++ ")"
stringify (Leaf a) = show a

treesize (Branch x y z) = (treesize x) + 1 + (treesize z)
treesize (Leaf a) = 1

elements (Leaf a) = [a]
elements (Branch x y z) = (elements x) ++ [y] ++ (elements z)

treefold op id (Leaf a) = id a
treefold op id (Branch x y z) = ( op (treefold op id x) (id y) (treefold op id z))

elts2 = treefold (\x y z -> x ++ y ++ z) (\x -> [x])

-- treefold op (Leaf a) = a
-- treefold op (Branch x y z) = (op (treefold op x) y (treefold op z))
-- zerotreefold op zero btree = op zero (treefold op btree) zero


data Color 
    = Red
    | Orange
    | Yellow
    | Green
    | Custom Int Int Int

colorToRGB Red = (255,0,0)
colorToRGB Orange = (255,128,0)
colorToRGB Yellow = (255,255,0)
colorToRGB (Custom x y z) = (x,y,z)


main=do
  print (colorToRGB (Custom 1 2 3))
  print (treefold (\x y z -> x+y+z) id (listtotree [1..5]))
  print (elts2 (listtotree [1..5]))
  print (stringify (listtotree [1,2,3,4,5]))
  print (treesize (listtotree [1,2,3,4,5]))
  print (elements (listtotree [1,2,3,4,5]))
  print (foldl (++) [] (elements (listtotree ["a","b","c","d","f"])))
  print (listHead samplelist)
  print (listHead(listTail(samplelist)))
  putStrLn (show (split (Triple 2 3 4)))
  putStrLn (show (split (Triple 'a' 'b' 'c')))
  putStrLn (show (firstTwo (Quadruple 'a' 'b' 2 3)))
  putStrLn (show (lastTwo (Quadruple 'a' 'b' 2 3)))
  let tuplelist = [(T4 'a' 'b' 'c' 'd'), (T3 'x' 'y' 'z' ), (T2 'x' 'y'), (T1 '2')]
  putStrLn (show (map (\x -> (tuple1 x, tuple2 x, tuple3 x, tuple4 x)) tuplelist))
  putStrLn (show (map (\x -> (tuple1 x, tuple2 x, tuple3 x, tuple4 x)) tuplelist))
  putStrLn (show (map tuplize tuplelist))



{- working for foldl (which is a tail recursion in evalution order)
    (fl + 0 '(10 3 7))
 -> (fl + (+ 0 10) '(3 7))
 -> (fl + 10 '(3 7))
 -> (fl + (+ 10 3) '(7))
 -> (fl + 13 '(7))
 -> (fl + (+ 13 7) '())
 -> (fl + 20 '())
 -> 20

 (fl + 0 '(10 3 7)) (in normal order)
 -> (fl + (+ 0 10) '(3 7))
 -> (fl + (+ (+ 0 10) 3) '(7))
 -> (fl + (+ (+ (+ 0 10) 3) 7) '())
 -> (+ (+ (+ 0 10) 3) 7)
 -> (+ (+ 10 3) 7)
 -> (+ 13 7)
 -> 20
-}

{- working for foldr
  (fr + 0 '(10 3 7))
--> (+ 10 (fr + 0 '(3 7)))
--> (+ 10 (+ 3 (fr + 0 '(7))))
--> (+ 10 (+ 3 (+ 7 0)))
-}



