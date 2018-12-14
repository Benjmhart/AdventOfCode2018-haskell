import Data.List
import Data.Char
import Control.Applicative((<$>), (<*>))
import Data.Maybe
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Debug.Trace

t' str x = trace ("\n"++ str ++ " " ++ (show x)) x

type Scores = IntMap Int

--the mapping and the length, tracked without calculating size (addition baby)

-- Item Key in ScoreBoard & value of current Item
type Elf = (Int, Int) 

type TriggerTime = Int

type ScoreL = Int

data GameState = GameState Elf Elf Scores ScoreL
  deriving (Show, Eq, Ord)

--step 1 - initialize the game with 3, 7
--step 2 - iterate the game until timer == input+10
--step 3 - query the ScoreBoard for the 10 last entries & fromJust them


main :: IO()
main = do
  input <- readFile $ "./inputs/day14-1.txt"
  putStrLn $ solve . read $ input
  return ()

initialState :: GameState
initialState = GameState (0, 3) (1, 7) scores 2
  where scores = IM.insert 1 7 . IM.insert 0 3 $ IM.empty

solve :: Int -> String
solve n = answer
  where 
    answer       = concat . map show . catMaybes $ keyLookup <$> lookupList
    lookupList   = [n..n+9]
    resultScores = getScores . evolveState' n $ initialState
    keyLookup x  = IM.lookup x resultScores

evolveState' :: Int -> GameState -> GameState
evolveState' n state
  | nextScoreL > (n+10) = nextState
  | otherwise           = evolveState' n nextState
  where 
    nextScoreL = getScoreL nextState
    nextState  = evolveState state

getScoreL :: GameState -> ScoreL
getScoreL (GameState _ _ _ scoreL) = scoreL

getScores :: GameState -> Scores
getScores (GameState _ _ scores _) = scores

evolveState :: GameState -> GameState
evolveState (GameState e1@(_, v1) e2@(_, v2) scores scoreL)
  = GameState nextE1 nextE2 nextScores nextScoreL
  where
    nextE1 = elfStep e1 nextScoreL nextScores
    nextE2 = elfStep e2 nextScoreL nextScores
    (nextScores, nextScoreL) = addScores nextScoreList scores scoreL
    nextScoreList            =  getNextScores v1 v2

-- takes the nextScoreL so that it knows where to loop around
elfStep :: Elf -> ScoreL -> Scores -> Elf
elfStep (i, v) nextScoreL scores = (nextPosition, nextValue)
  where
    nextPosition = ((i+(v+1)) `mod` nextScoreL )
    nextValue    = fromJust $ IM.lookup nextPosition scores


addScores :: [Int] -> Scores -> ScoreL -> (Scores, ScoreL)
addScores []     scores scoreL = (scores, scoreL)
addScores (x:xs) scores scoreL = addScores xs nextScores nextL
  where
    nextScores = IM.insert scoreL x scores
    nextL      = scoreL +1

getNextScores :: Int -> Int -> [Int]
getNextScores n1 n2 = map ( read . (:[])) . show $ n1+n2

 



