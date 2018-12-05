import Data.Char
import Data.List
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

solve :: String -> Int
solve xs = length shortest
  where shortest = last . sortBy sortLength . map collapse $ variants
        variants = getVariants xs

sortLength :: [a] -> [a] -> Ordering
sortLength a b
  | length a == length b = EQ  
  | length a <  length b = GT
  | length a >  length b = LT

pairs = [ ('a', 'A')
        , ('b', 'B')
        , ('c', 'C')
        , ('d', 'D')
        , ('e', 'E')
        , ('f', 'F')
        , ('g', 'G')
        , ('h', 'H')
        , ('i', 'I')
        , ('j', 'J')
        , ('k', 'K')
        , ('l', 'L')
        , ('m', 'M')
        , ('n', 'N')
        , ('o', 'O')
        , ('p', 'P')
        , ('q', 'Q')
        , ('r', 'R')
        , ('s', 'S')
        , ('t', 'T')
        , ('u', 'U')
        , ('v', 'V')
        , ('w', 'W')
        , ('x', 'X')
        , ('y', 'Y')
        , ('z', 'Z') ]

getVariants :: String -> [String]
getVariants xs = map (\(c1, c2) -> filter (filterPair (c1, c2)) xs)  pairs

filterPair :: (Char, Char) ->Char -> Bool
filterPair (c1, c2) match = not $ match == c1 || match == c2 

collapse:: String -> String
collapse xs = deleted 
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