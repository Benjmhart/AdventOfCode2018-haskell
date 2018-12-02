
import Data.Maybe(fromJust)
import Data.Set(Set(..), empty, insert, member, toList)

main :: IO()
main =  do
  intStr <- readFile $ "./inputs/day1-2.txt"
  let ints = map toInt . words $ intStr
  putStr "Beginning thing"
  putStr . show . solve $ ints
  return ()
  where toInt x = read x :: Integer

solve :: [Integer] -> Integer
solve xs = solve' (0, (insert 0 empty :: Set Integer), Nothing) xs

solve' :: (Integer, Set Integer, Maybe Integer) -> [Integer] -> Integer
solve' acc xs
  | dupe == Nothing  = solve' freqs xs
  | otherwise        = fromJust dupe
  where dupe  = thd freqs
        freqs = getFreqs acc xs

thd :: (a,b,c)->c 
thd (_,_,x) = x


getFreqs :: (Integer , Set Integer, Maybe Integer) -> [Integer] -> (Integer, Set Integer, Maybe Integer)
getFreqs acc integerList = foldl addToLast acc integerList
  where addToLast :: (Integer, Set Integer, Maybe Integer) -> Integer -> (Integer, Set Integer, Maybe Integer)
        addToLast (a,b, Just x) _ = (a,b, Just x)
        addToLast (lastVal, setA, Nothing) nextVal
          | newValue `member` setA = (lastVal, setA, Just newValue)
          | otherwise              = (newValue, newSet, Nothing)
          where newValue = lastVal + nextVal
                newSet   = insert newValue setA

