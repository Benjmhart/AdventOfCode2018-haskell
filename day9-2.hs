import Data.List
import Data.Char
import Control.Applicative((<$>), (<*>))
import Data.Maybe
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Debug.Trace

t' x = traceShow x x

t'' str x = trace (str ++ " " ++ (show x)) x

t''' str s@(_, _, _, _, xs) = trace (str ++ " " ++ (show xs)) s

type Game  = (Int, Int) -- players, last marble, highscore
type State = (Int, Int, Int, [Int], (IntMap Integer))  -- cPlayer, cMarble, nMarble [marbles]. [scores] 

main :: IO()
main = do
  input <- readFile $ "./inputs/day9-1.txt"
  putStr $ (++ "\n"). show . solve $ input
  return ()



solve :: String -> Integer
solve = last .  getScores . trace "finished game" . playGame' . makeGame
  
getScores :: State -> [Integer]
getScores (_,_,_,_, scores) = sort . map snd . M.toList $ scores

playGame' :: Game -> State
playGame' game@(p, lmv) = playGame (p, lmv*100) ((-1), 0, 1, [0], M.empty)

playGame :: Game -> State -> State
playGame 
  game@(players, lastMarbleValue) 
  state@(_, _, next, _, scores)
    | gameEnd   = state
    | otherwise = playGame game nextState
    where 
      gameEnd   = (next) > lastMarbleValue
      nextState = turn players state

safetyHead [] = (0,0)
safetyHead xs = head xs

-- scores will be a list of playerId and individual score
-- player ID will be 1-based, and resets to 1
turn :: Int -> State -> State
turn _ ((-1), 0, 1, [0], scores)
  = ( 1, 1, 2, [0, 1], scores)
turn players state@(currentPlayer, currentMarble, nextMarble, marbleCircle, scores)
  | hasScore  = stateWithScore
  | otherwise = (nextPlayer, nextCurrentMarble, nextNMarb, nextCircle, scores )
  where hasScore           = (nextMarble) `mod` 23 == 0
        nextPlayer         = getNextPlayer players currentPlayer 
        nextCurrentMarble  = nextMarble
        nextNMarb          = nextMarble + 1 
        nextCircle         = insertAt insertIndex nextMarble marbleCircle
        stateWithScore     = getScoreState players state
        insertIndex        = cycleIndex currentMarbleIndex 2 (length marbleCircle) 
        currentMarbleIndex = fromJust $ maybecmi
        maybecmi           = currentMarble `elemIndex` marbleCircle

--getNextPlayer = top level function used by both

getScoreState :: Int -> State -> State
getScoreState players (currentPlayer, currentMarble, nextMarble, marbleCircle, scores)
  = (nextPlayer, nextCurrentMarble, nextNMarb, nextCircle, nextScores)
    where
      currentMarbleIndex = fromJust $ currentMarble `elemIndex` marbleCircle
      removalIndex       = cycleIndex currentMarbleIndex (-7) cl
      removedMarble      = marbleCircle !! removalIndex
      nextCircle         = delete removedMarble marbleCircle
      nextCurrentIndex   = removalIndex -- cycleIndex removalIndex (-1) cl
      nextCurrentMarble  = nextCircle !! nextCurrentIndex
      nextPlayer         = getNextPlayer players currentPlayer 
      cl                 = length marbleCircle
      scoreAmt           = t'' "inserting" . fromIntegral $ nextMarble + removedMarble
      nextScores = M.insertWith (+) currentPlayer scoreAmt scores 
      nextNMarb          = nextMarble + 1

getNextPlayer :: Int -> Int -> Int
getNextPlayer players currentPlayer 
  = if posNextPlayer == 0 then players else posNextPlayer  
    where posNextPlayer = (currentPlayer + 1) `mod` players

cycleIndex :: Int -> Int -> Int -> Int
cycleIndex start n circleSize
  | modResult == 0 = circleSize
  | modResult <  0 = abs modResult
  | otherwise      = modResult
  where modResult  = (start + n) `mod` circleSize
        

insertAt :: Int -> a -> [a] -> [a]
insertAt n e xs = ys ++ [e] ++ zs
  where (ys, zs) = splitAt n xs

makeGame :: String -> Game
makeGame = toPair . map read . words . filter ((||) <$> isDigit <*> isSpace)

toPair :: [a] -> (a,a)
toPair (x:y:_) = (x,y)