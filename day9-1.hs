import Data.List
import Data.Char
import Control.Applicative((<$>), (<*>))
import Control.Monad
import Data.Maybe
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import qualified Data.List.PointedList.Circular as C
import Data.List.PointedList.Circular (PointedList)

type Game  = (Int, Int) -- players, last marble, highscore
type State = (Int, Int, Int, PointedList Int, (IntMap Int))  -- cPlayer, cMarble, nMarble [marbles]. [scores] 

main :: IO()
main = do
  input <- readFile $ "./inputs/day9-1.txt"
  putStr $ (++ "\n"). show . solve $ input
  return ()

solve :: String -> Int
solve = maximum .  getScores . playGame' . makeGame
  
getScores :: State -> [Int]
getScores (_,_,_,_, scores) =  map snd . M.toList $ scores

playGame' :: Game -> State
playGame' game@(p, lmv) = playGame (p, lmv) ((-1), 0, 1, C.singleton 0, M.empty)

playGame :: Game -> State -> State
playGame 
  game@(players, lastMarbleValue) 
  state@(_, _, next, _, scores)
    | gameEnd   = state
    | otherwise = playGame game nextState
    where 
      gameEnd   = (next) > lastMarbleValue
      nextState = turn players state


turn :: Int -> State -> State
turn _ ((-1), 0, 1, circle, scores)
  = ( 1, 1, 2, (C.insert 1 circle), scores)
turn players state@(currentPlayer, currentMarble, nextMarble, marbleCircle, scores)
  | hasScore  = stateWithScore
  | otherwise = (nextPlayer, nextCurrentMarble, nextNMarb, nextCircle, scores )
  where hasScore           = (nextMarble) `mod` 23 == 0
        nextPlayer         = getNextPlayer players currentPlayer 
        nextCurrentMarble  = nextMarble
        nextNMarb          = nextMarble + 1 
        nextCircle         = C.insert nextMarble . C.next $ marbleCircle
        stateWithScore     = getScoreState players state

getScoreState :: Int -> State -> State
getScoreState players (currentPlayer, currentMarble, nextMarble, marbleCircle, scores)
  = (nextPlayer, nextCurrentMarble, nextNMarb, circ, nextScores)
    where
      (removed, Just circ ) = liftM2 (,) C._focus C.delete (C.moveN (-7) marbleCircle)
      nextPlayer         = getNextPlayer players currentPlayer 
      scoreAmt           = nextMarble + removed
      nextCurrentMarble  = C._focus circ
      nextScores = M.insertWith (+) currentPlayer scoreAmt scores 
      nextNMarb          = nextMarble + 1

getNextPlayer :: Int -> Int -> Int
getNextPlayer players currentPlayer 
  = if posNextPlayer == 0 then players else posNextPlayer  
    where posNextPlayer = (currentPlayer + 1) `mod` players

makeGame :: String -> Game
makeGame = toPair . map read . words . filter ((||) <$> isDigit <*> isSpace)

toPair :: [a] -> (a,a)
toPair (x:y:_) = (x,y)