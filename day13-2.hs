import Data.List
import Data.Char
import Control.Applicative((<$>), (<*>))
import Control.Monad
import Control.Arrow
import Data.Maybe
import qualified Data.Map as M
import Debug.Trace

t' x = traceShow x x


type Coord = (Int,Int)

type TrackMap = M.Map Coord Char

data Direction = North | East | South | West deriving (Eq, Ord, Show)

data NextTurn = Strt | Lft | Rgt deriving (Eq, Ord, Show)
 
data Cart = Cart Coord Direction NextTurn deriving (Eq, Ord, Show)

safeHead = listToMaybe
-- BoardState always shows a map of all tracks, a list of carts that have not yet moved, and a list of carts that have moved, and a maybe collision Coord
data BoardState = BoardState TrackMap [Cart] [Cart] (Maybe Coord) deriving (Eq, Ord, Show)

main :: IO()
main = do
  input <- readFile $ "./inputs/day13-1.txt"
  cycleSolve . parse $ input

cycleSolve :: BoardState -> IO ()
cycleSolve bs = do
  --putStrLn $ showBoardState bs
  if hasCrash bs
    then do
      putStrLn $ "crash: " ++ (show . getCrash $ bs)
      let nextbs = processCrash bs
      if thereCanBeOnlyOne nextbs
        then do
          cycleSolve nextbs
        else do
          let highlander = getLast nextbs
          putStrLn $ "CHAMPION OF THE WORLD: " ++ (show $ highlander)
          return ()
    else cycleSolve nextBoardState
  where 
    nextBoardState = moveCart bs
    thereCanBeOnlyOne :: BoardState -> Bool
    thereCanBeOnlyOne (BoardState _ uc mc _) = (length acs) > 1
      where acs =(uc ++ mc)
    getLast :: BoardState -> Coord
    getLast bs@(BoardState _ uc mc _) 
      | null mc = getLast . moveCart $ bs -- tick is not complete. spoon!
      | otherwise = (y, x) -- the quickening!
        where (Cart (x, y) _ _) = head mc
    hasCrash :: BoardState -> Bool
    hasCrash (BoardState _ _ _ crash)
      | isJust crash = True
      | otherwise    = False
    getCrash :: BoardState -> Coord
    getCrash (BoardState _ _ _ Nothing) = error "grumble grumble"
    getCrash (BoardState _ _ _ (Just (x, y))) = (y,x) --stupid Coord system




processCrash :: BoardState -> BoardState
processCrash (BoardState trackMap unmoved moved crash)
  = BoardState trackMap nextUnmoved nextMoved Nothing
    where
      crashCoords = fromJust crash
      nextUnmoved = filterCrashed unmoved
      nextMoved   = filterCrashed moved
      filterCrashed x = filter ((/=crashCoords) . getCoordsFromCart) x


showBoardState:: BoardState -> String
showBoardState (BoardState trackMap unmoved moved crash)
  = "\nnext board\nmoved:\n"++  (unlines . map show $ moved) ++ "\nunmoved: \n"++(unlines . map show $ unmoved) ++"crash: " ++ show crash
  where allcarts = unmoved ++ moved


moveCart :: BoardState -> BoardState
moveCart (BoardState trackMap [] moved Nothing)
  = BoardState trackMap (sortBy cartSort $ moved) [] Nothing
moveCart bs@(BoardState trackMap (cart@(Cart (x,y) dir turn):unmoved) moved crash)
  | isCrash     = crash
  | otherwise   = nextBoard
  where
    crash     = BoardState trackMap unmoved (nextCart:moved) (Just dest)
    nextBoard = BoardState trackMap unmoved (nextCart:moved) Nothing
    isCrash   = (dest `elem`) . fmap getCoordsFromCart $ unmoved ++ moved
    currentSquare            = (x,y) `M.lookup` trackMap
    nextCart@(Cart dest _ _) = getNextCart bs currentSquare

cartSort :: Cart -> Cart -> Ordering
cartSort (Cart (x1,y1) _ _) (Cart (x2, y2) _ _)
  | x1 < x2 = LT
  | x1 > x2 = GT
  | y1 < y2 = LT
  | y1 > y2 = GT
  | otherwise = EQ

getCoordsFromCart :: Cart -> Coord
getCoordsFromCart (Cart coords _ _) = coords

getNextCart :: BoardState -> Maybe Char -> Cart
getNextCart (BoardState _ (cart@(Cart (x,y) dir turn):_) _ _) c 
  | c == (Just '+')                  = processTurn cart
  | c == (Just '|')  && dir == North = Cart (x-1, y) North turn
  | c == (Just '|')  && dir == South = Cart (x+1, y) South turn
  | c == (Just '-')  && dir == East  = Cart (x, y+1) East  turn
  | c == (Just '-')  && dir == West  = Cart (x, y-1) West  turn
  | c == (Just '/')  && dir == West  = Cart (x+1, y) South turn
  | c == (Just '/')  && dir == North = Cart (x, y+1) East  turn
  | c == (Just '/')  && dir == South = Cart (x, y-1) West  turn
  | c == (Just '/')  && dir == East  = Cart (x-1, y) North turn
  | c == (Just '\\') && dir == West  = Cart (x-1, y) North turn
  | c == (Just '\\') && dir == North = Cart (x, y-1) West  turn
  | c == (Just '\\') && dir == South = Cart (x, y+1) East  turn
  | c == (Just '\\') && dir == East  = Cart (x+1, y) South turn
  | otherwise = error "you dun goofed"
  where
    processTurn :: Cart -> Cart
    processTurn (Cart (x,y) dir turn)
      | dir == North && turn == Strt = Cart (x-1, y) North Rgt
      | dir == North && turn == Lft  = Cart (x, y-1) West Strt
      | dir == North && turn == Rgt  = Cart (x, y+1) East Lft
      | dir == South && turn == Strt = Cart (x+1, y) South Rgt
      | dir == South && turn == Rgt  = Cart (x, y-1) West Lft
      | dir == South && turn == Lft  = Cart (x, y+1) East Strt
      | dir == East  && turn == Strt = Cart (x, y+1) East Rgt
      | dir == East  && turn == Rgt  = Cart (x+1, y) South Lft
      | dir == East  && turn == Lft  = Cart (x-1, y) North Strt
      | dir == West  && turn == Strt = Cart (x, y-1) West Rgt
      | dir == West  && turn == Rgt  = Cart (x-1, y) North Lft
      | dir == West  && turn == Lft  = Cart (x+1, y) South Strt
      | otherwise = error "game over"

parse :: String -> BoardState
parse rails = BoardState trackMap cartsList [] Nothing
  where 
    cartsList = fmap parseCart . filter (isCart . snd) $ coordList
    trackMap       = M.fromList $ (makeTrackChars `second`) <$> coordList
    makeTrackChars :: Char -> Char
    makeTrackChars c
      | c == '<'  = '-'
      | c == '>'  = '-'
      | c == '^'  = '|'
      | c == 'v'  = '|'
      | otherwise =  c
    coordList     = zip allCoords . filter (/= '\n') $ rails
    allCoords     = [ (x,y)
                     | x <- [0..((length (lines rails)) -1)]
                     , y <- [0..((getLineLength  0 rails) - 1 )] ]
    isCart :: Char -> Bool
    isCart c
      | c == '<'  = True
      | c == '>'  = True
      | c == '^'  = True
      | c == 'v'  = True
      | otherwise = False
    parseCart :: (Coord, Char) -> Cart
    parseCart (coord, c)
      | c == '<'  = Cart coord West  Lft
      | c == '>'  = Cart coord East  Lft
      | c == '^'  = Cart coord North Lft
      | c == 'v'  = Cart coord South Lft
      | otherwise = error "list not properly cleansed for parsing"




getLineLength :: Int -> String -> Int
getLineLength n []       = n
getLineLength n ('\n':_) = n
getLineLength n (x:xs)   = getLineLength (n+1) xs
