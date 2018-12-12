import Data.List
import Data.Char
import Control.Applicative((<$>), (<*>))
import Control.Arrow ((&&&), (***))
import Debug.Trace

t' x = trace x x

t'' str x = trace (str ++ (show . length $ x)) x


main :: IO()
main = do
  input <- readFile $ "./inputs/day12-1.txt"
  putStrLn $ solve 20 . tail . tail . words $ input
  return ()


-- # generations,  (state:patterns) -> sum of indexes with plants
solve :: Int -> [String] -> String
solve n (initialState:patterns) 
  = show . sum . map snd . filter ((=='#') . fst) $ indexed
    where
      indexed = zip finalGen [negs..]
      finalGen = solvePadding n len patterns $ paddedState
      paddedState = padding ++ initialState ++ padding
      padding = take padNum $ repeat '.'
      padNum = n + 3
      negs = neg $ (padNum )
      neg n = n - n - n 
      len = length paddedState

solvePadding :: Int -> Int-> [(String)] -> String-> String
solvePadding n len patterns state
  | n == 0    = state
  | otherwise = solvePadding (n-1) len patterns result 
  where 
    result = reverse . evolveState state $ ".."
    evolveState :: String-> String-> String
    evolveState []    acc 
      | length acc > len = evolveState [] (tail acc)
      | length acc < len = evolveState [] acc ++ "." 
      | otherwise        = acc 
    evolveState state acc
      | test `elem` patterns = evolveState (tail state) ('#':acc)
      | otherwise            = evolveState (tail state) ('.':acc)
      where test = take 5 $ state