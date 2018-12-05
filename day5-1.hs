
import Data.Char
import Debug.Trace

traceShow' x = traceShow x x

main :: IO()
main = do
  input <- readFile $ "./inputs/day5-1.txt"
  putStr $ (++ "\n"). show . solve $ input
  return ()

testStr1 = "aA" -- 0
testStr2 = "abBA" -- 0
testStr3 = reverse "aA" -- 0
testStr4 = reverse "abBA" -- 0
testStr5 = "abAB" --4
testStr6 = "aabAAB"--6
testStr7 = "dabAcCaCBAcCcaDA"

solve:: String -> Int
solve xs = length $ deleted 
  where deleted = operateUntilDone xs

operateUntilDone :: String -> String
operateUntilDone xs
  | xs == op  = xs
  | otherwise = operateUntilDone op
  where op = deletePairs xs

deletePairs :: String -> String
deletePairs []  = []
deletePairs [x] = [x]
deletePairs (c:x:xs)
  | x == target = xs 
  | otherwise   = c:(deletePairs (x:xs))
  where target = if isLower c then toUpper c else toLower c