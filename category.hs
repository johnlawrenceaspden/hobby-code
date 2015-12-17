-- http://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/

import Data.Char

-- First get haskell itself working
-- $ sudo apt-get install ghc rlwrap
-- $ rlwrap ghci
-- Prelude> 2*3
-- Prelude> reverse "dlrow olleh"
-- Prelude> product [1 .. 5]
-- Prelude> product [1 .. 5]
-- Prelude> readFile "/home/john/.profile"
-- Prelude> do line <- getLine; putStrLn line

-- And now you can load this file
-- Prelude> :load category.hs
-- *Main> ident 3

-- or you can go straight to *Main> with
-- $ ghci category.hs

-- Getting things to work with emacs is a bit of a movable feast as always.
-- This page used to be right, but isn't now
-- https://wiki.haskell.org/Emacs/Inferior_Haskell_processes


-- Chapter One: Category, the essence of composition

-- A category is a graph where all nodes have self loops, and a->b, b->c => a->c
-- so like an equivalence relation but without symmetry

-- An example is sets and the functions between them. There is always an identity function, and any two functions f:A->B and g:B->C compose to give g.f:A->C



ident :: t -> t
ident x = x


square :: Num a => a -> a
square x = x*x

ab :: Integral a => a -> [Char]
ab x = if mod x 2 ==0 then "a" else "b"

bar :: [Char]
bar = (ab (square 3))

foo :: Integer
foo = (ident square) 3

s1t20 :: Integer
s1t20 = foldr (+) 0 [1..20]


-- Chapter 4: Kleisli Categories

type Writer a = (a,String)

-- Composition of two arrows is like function composition but with extra structure
(>=>) :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)
m1 >=> m2 = \x ->
  let (y,s1) = m1 x
      (z,s2) = m2 y
  in (z, s1 ++ s2)

-- Identity arrow
return :: a -> Writer a
return x = (x,"")

-- And here are two arrows (morphisms) 
upCase :: String -> Writer String
upCase s = (map toUpper s, "upcase ")

toWords :: String -> Writer [String]
toWords s = (words s, "toWords ")

-- And another arrow defined as their composition
process :: String -> Writer [String]
process = upCase >=> toWords




-- tracing fibs
-- Integer is the general type, Int is the machine numbers?
-- Want to say something like:
--((tfib >=> tfib >=> tplus) (n-1) (n-2),"fib")
fib :: Integer -> Integer
fib n = if (n<2) then n else fib (n-1) + fib (n-2)

tplus :: Integer -> Integer ->Writer Integer
tplus x y = (x+y,"tplus ")

tfib :: Integer -> Writer Integer
tfib n = if (n<2) then (n,"fib01 ") else
  let (f1,s1) = tfib (n-1)
      (f2,s2) = tfib (n-2)
      (ans,s3) = tplus f1 f2
  in (ans,s1++s2++s3++"tfib ")

-- Safe Inverse/Safe sqrt exercise

data CanFail a = Value a | Fail deriving (Show)

safeInv :: Double -> CanFail Double
safeInv x = if (x==0) then Fail else Value (1/x)

safeSqrt :: Double -> CanFail Double
safeSqrt x = if x<0 then Fail else Value (sqrt x)

(>==>) :: (a -> CanFail b) -> (b -> CanFail c) -> (a -> CanFail c)
--(>==>) f g = \x -> (let a1 = (f x) in (if (a1==Fail) then Fail else (g a1)))
(>==>) f g = \x -> (let a1 = (f x) in
                     (case a1 of Fail    -> Fail
                                 Value x -> (g x)))

gloriousvictory=map (safeInv >==> safeSqrt) [4,3,2,1,0,-1,-2]

rah = do
       print gloriousvictory


-- Chapter 5: Products and CoProducts
first :: (a,b)-> a
first (x,y) = x

second :: (a,b)-> b
second (x,y) = y























