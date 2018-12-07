import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

t' str x = trace (str ++ (show x) ++ "\n") x

main :: IO()
main = do
  input <- readFile $ "./inputs/day7-1.txt"
  putStr $ (++ "\n").  solve $ input
  return ()

sortFile :: IO()
sortFile = do
  input <- readFile $ "./inputs/day7-1.txt"
  let sorted = unlines . sort . lines $ input
  writeFile "./inputs/day7-1sorted.txt" sorted

lastLetter = 'Z' :: Char 

type CharPair = (Char, Char)

solve :: String -> String
solve xs = reverse $ solve' rules allMoves []
  where 
    allMoves = ['A'..lastLetter]
    rules = map extract . lines $ xs
    extract ys = (ys!!5, ys !!36)

solve' :: [CharPair] -> [Char]-> [Char] -> [Char]
solve' []    _     movesUsed = movesUsed
solve' _     []    movesUsed = movesUsed
solve' rules movesNotUsed [] = solve' rules movesNotUsed [firstMove]
  where firstMove = t' "first" $ head [ m | m <- movesNotUsed, not $ m `elem` locked]
        locked = map snd rules
solve' rules movesNotUsed movesUsed@(mu:_) = solve' nextRules nextNotUsed nextUsed
  where
    nextRules   =  filter ((/= mu) . fst) rules
    nextNotUsed =  delete (mu) movesNotUsed
    nextUsed    =  (move:movesUsed)
    move        = if length candidates > 0 then head candidates else last nextNotUsed
    locked       = map snd nextRules
    candidates   = [ m 
                   | m <-nextNotUsed
                   , not $ m `elem` locked]  
    

    -- Find the letter not in the fst list - this is the last move
    -- Find the requirements for the last move select one that comes last alphabetically