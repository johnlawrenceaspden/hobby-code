module Main
    where

import IO
import Random

askForNumbers = do
  putStrLn "enter a number:"
  guess <- getLine
  let num = read guess
  if num == 0 
    then return []
    else do
      nl <- askForNumbers
      return (num: nl)

my_factorial 0 = 1
my_factorial n = n * my_factorial (n - 1)

factorial_print list =
    if (not (null list))
    then do
      putStrLn ("the factorial of " ++ 
                (show (head list)) ++ " is " ++ (show (my_factorial (head list))))
      do
        factorial_print (tail list)
    else
      do
        putStrLn "dummy"

myprog = do
  fluffy <- askForNumbers
  let sum  = foldr (+) 0 fluffy
  let prod = foldr (*) 1 fluffy
  putStrLn (show fluffy)
  putStrLn ("sum: " ++ (show sum))
  putStrLn ("product: " ++ (show prod))
  factorial_print fluffy
  let facts = map my_factorial fluffy
  putStrLn ("factorials: " ++ (show facts))


main = do
  hSetBuffering stdin LineBuffering
  putStrLn "Enter a bunch of numbers and terminate with zero:"
  myprog






