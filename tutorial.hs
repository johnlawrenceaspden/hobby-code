--- The Broken Haskell Tutorial from Lisperati
--- http://lisperati.com/haskell/ht1.html

--- run with
--- runhaskell tutorial.hs

import Data.List
import Data.Ord
--- sudo apt-get install libghc-regex-posix-dev
--- apparently on ubuntu 13.04 the haskell-platform is broken so:
--- sudo apt-get install ghc alex cabal-install happy libghc-cgi-dev libghc-fgl-dev libghc-glut-dev libghc-haskell-src-dev libghc-html-dev libghc-http-dev libghc-hunit-dev libghc-mtl-dev libghc-network-dev libghc-opengl-dev libghc-parallel-dev libghc-parsec3-dev  libghc-quickcheck2-dev libghc-regex-base-dev libghc-regex-compat-dev  libghc-regex-posix-dev libghc-stm-dev libghc-syb-dev  libghc-text-dev  libghc-transformers-dev  libghc-xhtml-dev libghc-zlib-dev

import Text.Regex
import System.Random

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Polygon   = [Point]
type Person    = [Int]
type Link      = [Point]
type Placement = [(Point,Person)]

type EnergyFunction a              = a -> Int
type TemperatureFunction           = Int -> Int -> Float
type TransitionProbabilityFunction = Int -> Int -> Float -> Float
type MotionFunction a              = StdGen -> a -> (StdGen,a)

main = do
  putStr "Hello World! Let's have a picnic! \n"

  people_text <- readFile "people.txt"

  let people :: [Person]
      people = read people_text

  putStr "Number of people coming: "
  print (length people)

  let writePoint :: Point -> String
      writePoint (x,y) = (show x)++","++(show y)++" "

  let writePolygon :: (Color,Polygon) -> String
      writePolygon ((r,g,b),p) = "<polygon points=\""++(concatMap writePoint p)++"\" style=\"fill:#cccccc;stroke:rgb("++(show r)++","++(show g)++","++(show b)++");stroke-width:2\"/>"

  let writePolygons :: [(Color,Polygon)] -> String
      writePolygons p = "<svg xmlns=\"http://www.w3.org/2000/svg\">"++(concatMap writePolygon p)++"</svg>"

  let colorize :: Color -> [Polygon] -> [(Color,Polygon)]
      colorize = zip.repeat

  let rainbow@[red,green,blue,yellow,purple,teal] = map colorize [(255,0,0),(0,255,0),(0,0,255),(255,255,0),(255,0,255),(0,255,255)]

  writeFile "tut0.svg" $ writePolygons (blue [[(100,100),(200,100),(200,200),(100,200)],[(200,200),(300,200),(300,300),(200,300)]])

  let readPoint :: String -> Point
      readPoint s | Just [x,y] <- matchRegex (mkRegex "([0-9.]+),([0-9.]+)") s = (read x,read y)

  let readPolygon :: String -> Polygon
      readPolygon = (map readPoint).(splitRegex $ mkRegex " L ")

  let readPolygons :: String -> [Polygon]
      readPolygons = (map readPolygon).tail.(splitRegex $ mkRegex "<path")

  park_data <- readFile "park.svg" 
                      
  let park = readPolygons park_data

  writeFile "tut1.svg" $ writePolygons (green park)
