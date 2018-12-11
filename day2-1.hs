
import Data.List

main :: IO()
main =  do
  intStr <- readFile $ "./inputs/day2-1.txt"
  let items =  words $ intStr
  putStr . (++ "\n") . show . getCheckSum $ items
  return ()

getCheckSum :: [String] -> Integer
getCheckSum xxs = twos * threes
  where (twos, threes) = sumPairs . map hasTwoAndThree $ xxs

sumPairs :: [(Integer, Integer)] -> (Integer, Integer) 
sumPairs = foldl addPair (0,0)

addPair :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
addPair (x1,y1) (x2,y2) = (x1+x2, y1+y2)

hasTwoAndThree :: String -> (Integer, Integer)
hasTwoAndThree xs = traceShow' ((hasNum 2 xs xs), (hasNum 3 xs xs)) 

hasNum :: Int -> String -> String -> Integer
hasNum _ _   []     = 0
hasNum n str (x:xs)
  | dupeAmount == n = 1
  | otherwise       = hasNum n str xs
  where dupeAmount = length $ x `elemIndices` str 