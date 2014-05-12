--- The Broken Haskell Tutorial from Lisperati
--- http://lisperati.com/haskell/ht1.html

--- run with
--- runhaskell tutorial.hs 

import Data.List
import Data.Ord

--- import Text.Regex
--- import System.Random

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Polygon   = [Point]
type Person    = [Int]
type Link      = [Point]
type Placement = [(Point,Person)]

type EnergyFunction a              = a -> Int
type TemperatureFunction           = Int -> Int -> Float
type TransitionProbabilityFunction = Int -> Int -> Float -> Float
--- type MotionFunction a              = StdGen -> a -> (StdGen,a)

main = do 
  putStr "Hello World! Let's have a picnic! \n"
  
  people_text <- readFile "people.txt"

  let people :: [Person]
      people = read people_text

  putStr "Number of people coming: "
  print (length people)
