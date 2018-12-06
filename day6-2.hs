import Data.List
import Data.Char
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Monoid(Sum(..), getSum)
import Control.Applicative
import Debug.Trace

trace' x = traceShow x x 

main :: IO()
main = do
  input <- readFile $ "./inputs/day6-1.txt"
  let origins = parse input
  let areaList = maxArea origins
  let isWithinRange' = isWithinRange maxDist origins
  putStr $ (++ "\n"). show . length . filter isWithinRange' $ areaList
  return ()

type Coord = (Int, Int)

maxDist :: Int
maxDist = 10000

parse :: String -> [Coord]
parse xs = piped
 where
  piped = map pipeline . lines $ xs
  pipeline = toPair . map toInt . words . isNotPunctuation 
  isNotPunctuation = filter (not . isPunctuation)
  toInt x = read x :: Int
  toPair (a:b:[]) = (a, b)

maxArea :: [Coord] -> [Coord]
maxArea origins = liftA2 (,) [maxL..maxR] [maxT..maxB]
  where
    maxR = (+ maxDist) . minimum . map fst $ origins
    maxB = (+ maxDist) . minimum . map snd $ origins
    maxL = minus maxDist . maximum . map fst $ origins
    maxT = minus maxDist . maximum . map snd $ origins
    minus x y = y - x

isWithinRange :: Int -> [Coord] -> Coord -> Bool
isWithinRange distance origins coord = 
 (< distance) . getSum $ foldMap (Sum . getMHDistance coord) $ origins
        

getMHDistance :: Coord -> Coord -> Int
getMHDistance (x1, y1)  (x2, y2) = distance
 where distance = (abs(x1 - x2)) + (abs(y1 - y2))

