module Main
    where

x = 5
y = (6, "Hello")
z = x * fst y

main = putStrLn "Hello World"

square x = x * x

signum x = 
    if x < 0 
    then -1
    else if x > 0
         then 1
         else 0

f x = 
    case x of
      0 -> 1
      1 -> 5
      2 -> 2
      _ -> -1

g 0 = 1
g 1 = 5
g 2 = 2
g _ = -1

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

my_even 0 = True
my_even 1 = False
my_even n = not (my_even (n-1))

range n m =
    if n==m
    then [n]
    else n: (range (n+1) m)

evenfibs = (filter even (map fib (range 0 10)))

vxv="five squared is " ++ show (5*5)
eight = read "5" + 3

{-
  roots a b c =
  (( -b + sqrt (b*b - 4*a*c)) / (2*a),
  ( -b - sqrt (b*b - 4*a*c)) / (2*a))
-}

roots a b c =
    let 
        det = sqrt (b*b - 4*a*c) 
        two_a = 2*a
    in 
      (((-b + det) / two_a), ((-b - det) / two_a))



factorial 0 = 1
factorial n = n * factorial (n-1)


pascal 1 0 = 1
pascal 1 1 = 1
pascal 1 _ = 0
pascal row index = pascal (row-1) (index-1) + pascal (row-1) (index)

exponnt a 0 = 1
exponnt a b = a * exponnt a (b-1)

my_length [] = 0
my_length (x:xs) = 1 + my_length xs

my_filter pred [] = []
my_filter pred (x:xs) = 
    if (pred x)
    then 
        x: (my_filter pred xs)
    else
        my_filter pred xs


mult 0 b = 0
mult a b = b + mult (a - 1) b

my_map f [] = []
my_map f list = (f (head list)): (my_map f (tail list))