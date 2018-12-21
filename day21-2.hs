import Data.Bits
import Data.List
import Data.Ord
import Data.Maybe
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

-- generate an infinite list of valid D values
-- look for a pattern


main = do
  let allSolutions = emulate [65536, 0, 14906355, 0] 
  putStrLn $ show . take 5 $ allSolutions 
  putStrLn $ show $ solve allSolutions
  return ()



-- looks for the first repeat and selects the 
solve :: [Int] -> [Int]
solve = go [] IS.empty
  where
    go uniques seen (x:xs)
      | IS.member x seen = take 5 . nub $ uniques
      | otherwise = go (x:uniques) (IS.insert x seen) xs

-- creates an infinite list of solutions sorted by amount of operations
emulate :: [Int] -> [Int]
emulate (b:c:d:e:_)
  | addD      = tmpD:(emulate nextArgs)
  | otherwise = emulate [tmpB, tmpC, tmpD, tmpE2]
  where
    nextArgs = [tmpD .|. 65536, tmpC, 14906355, tmpE2]
    tmpE   = b .&. 255 
    tmpD   = ((((d + tmpE) .&. 16777215) * 65899) .&. 16777215)
    addD   = 256 > b
    tmpE2  = head aboveB
    aboveB = [ x | x <- [0..], (x + 1) * 256 > b]
    tmpC   = (tmpE2 + 1) *256
    tmpB   = tmpE2

  --  [3173684,14604476,12145300,6035420,7460084]
