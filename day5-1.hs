
import Data.Char
import Debug.Trace

traceShow' x = traceShow x x

main :: IO()
main = do
  input <- readFile $ "./inputs/day5-1.txt"
  putStr $ (++ "\n"). show . solve $ input
  return ()

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
  | x == target = deletePairs xs 
  | otherwise   = c:(deletePairs (x:xs))
  where target = if isLower c then toUpper c else toLower c