-- https://wiki.haskell.org/Emacs/Inferior_Haskell_processes
-- http://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/

-- (require inf-haskell)
-- C-c C-l to load ghci in another window and then let you interact

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
