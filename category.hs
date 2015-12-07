-- http://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/

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


