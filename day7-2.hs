import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S


main :: IO()
main = do
  input <- readFile $ "./inputs/day7-1.txt"
  putStr $ (++ "\n"). show . solve $ input
  return ()

lastLetter = 'Z' :: Char 
workers    = 5   :: Int

type CharPair = (Char, Char)
type Move = (Char, Int)    
data ProblemState = ProblemState { getTime      :: Int
                                 , workersList  :: [Move]
                                 , completeList :: [Char]} deriving(Show)

newState = ProblemState 0 [] []
--solve :: String -> String

setWL :: [Move] -> ProblemState -> ProblemState
setWL xs (ProblemState t _ cms) = (ProblemState t xs cms)

solve :: String -> Int
solve xs = getTime $ solve' rules allMoves newState
  where 
    allMoves   = zip ['A'..lastLetter] [61..]
    rules      = map extract . lines $ xs
    extract ys = (ys!!5, ys !!36)


solve' :: [CharPair] -> [(Char, Int)]-> ProblemState -> ProblemState
solve' []    _     ps@(ProblemState _ [] _) = ps
solve' []    u     ps@(ProblemState _ _ _)  = solve' [] u $ incProbTime ps
solve' _     []    ps@(ProblemState _ [] _) = ps
solve' r     []    ps@(ProblemState _ _ _)  = solve' r [] $ incProbTime ps
solve' rules movesNotUsed (ProblemState 0 [] [])  = solve' rules movesNotUsed initProb
  where initProb   = (ProblemState 0 firstMoves [])
        firstMoves = [ m | m <- movesNotUsed, not $ fst m `elem` locked]
        locked     = map snd rules
solve' rules movesNotUsed ps@(ProblemState t ws cms)
  | length ws >= workers   = solve' nextRules nextNotUsed moveAhead
  | length candidates == 0 = solve' nextRules nextNotUsed moveAhead
  | otherwise              = solve' nextRules nextNotUsed $ addMoves
  where
    moveAhead   = incProbTime ps
    addMoves    = setWL nextWorkers $ ps
    takeAmount  = take $ workers - (length ws)
    nextWorkers = ws ++ (takeAmount candidates)
    nextRules   = filter (not . (`elem` cms) . fst) rules
    nextNotUsed = filter (not . (`elem` ws)) movesNotUsed
    locked      = map snd nextRules
    currentJobs = map fst . filter ((/=1) . snd) $ ws
    candidates  = [ m 
                  | m <-nextNotUsed
                  , not $ fst m `elem` currentJobs
                  , not $ fst m `elem` locked]  
    
incProbTime :: ProblemState -> ProblemState
incProbTime (ProblemState t xs ys) = ProblemState (t+1) nextXs nextYs 
  where 
    nextYs               = (map fst completedTasks) ++ ys
    nextXs               = filter ((/=0) . snd) decreasedTasks
    completedTasks       = filter ((==0) . snd) decreasedTasks
    decreasedTasks       = map decTupleTime xs
    decTupleTime (c, t1) = (c, t1 - 1) 