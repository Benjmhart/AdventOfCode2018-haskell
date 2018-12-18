{-# LANGUAGE ViewPatterns #-}
import Data.Char
import System.IO (readFile)
import Data.Text (split, splitOn, pack, unpack)
import Data.List
import Data.Array.Unboxed (UArray, (!), (//))
import qualified Data.Array.Unboxed as UA
-- import Control.Applicative
import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as S


t' str x = trace ("\n"++ str ++ " " ++ (show x)) x
t'' x = traceShow x x
tr str x = trace str x

type Coord = (Int, Int)
type Board = UArray Coord Char
type Time  = Int
type ArrayBounds = (Coord, Coord)
type ResultSet = Set (Int, Int)
data PState = PState Board Time [Coord] ArrayBounds deriving (Eq, Ord)
instance Show PState where
  show (PState bd t _ _) = "\n Time: "++ (show t)++ " Board:\n"++ boardChars ++ "\n value: " ++ (show (treeVal * lumbVal))
    where 
      lumbVal    = length $ elemIndices '#' boardString
      treeVal    = length $ elemIndices '|' boardString
      (_, (_,w)) = UA.bounds bd
      boardChars = unlines . breakEvery w $ boardString
      boardString = UA.elems $ bd
      -- unlinez xs = unlines $ show <$> xs


-- program stops when you start getting repeats
-- the values become a sinusoidal wave near the 500th cycle
-- take the results of the last 50 steps, paste them into editor
-- search for the laste score it should appear on two lines
-- find the first appearance, record the time of the first appearance
-- subtract the time of the first appearance from the second appearance
-- call the time of your first appearance A
-- this is your interval
-- subtract A from the target (1billion)
-- take the result, and get result % inteval (modulus)
-- now take the modulus, and add it back to A call the new ime B
-- the score at the target is = the score at time B


main :: IO()
main = do
  input <- readFile $ "./inputs/day18-1.txt"
  let parsed = parse $ input
  -- putStrLn . show $ parsed
  resultSet <- cycleSolve 0 S.empty $ parsed
  putStrLn $ (show . S.size $ resultSet )
  return ()

cycleSolve :: Int -> ResultSet -> PState -> IO ResultSet
cycleSolve score results ps@(PState _ t _ _) = do
  let newScore = getLumbScore ps
  let diff = newScore - score
  putStrLn $ "time: " ++ (show t) ++" score: " ++ (show newScore) ++ " diff: " ++ (show $ diff)
  let nextPS = stepState ps
  let inserted = (S.insert (newScore, diff) results) 
  case (newScore, diff) `S.member` results of
    True -> return results
    False -> cycleSolve newScore inserted nextPS
    
  

getLumbScore :: PState -> Int
getLumbScore (PState bd _ _ _) = (treeVal * lumbVal)
  where
    lumbVal    = length $ elemIndices '#' boardString
    treeVal    = length $ elemIndices '|' boardString
    boardString = UA.elems $ bd

getTime :: PState -> Int
getTime (PState _ t _ _) = t

stepState :: PState -> PState
stepState  (PState bd t coords bds)
  = PState nextBd (t+1) coords bds
  where 
    nextBd     = foldl getNext freshArray coords
    freshArray = UA.array bds [] :: Board
    getNext :: Board -> Coord -> Board
    getNext bd2 co
      | openToTree = makeTree
      | treeToLumb = makeLumb
      | keepLumb   = makeLumb
      | loseLumb   = removeLumb
      | otherwise  = keepAsIs
      where
        keepAsIs   = bd2 // [(co, self)]
        removeLumb = bd2 // [(co, '.')]
        loseLumb   = self == '#' && not keepLumb
        keepLumb   = self == '#' && ('#' `elem` adj) && ('|' `elem` adj)
        makeLumb   = bd2 // [(co, '#')]
        treeToLumb = self == '|' && hasLumb
        hasLumb    = (>=3) . length . elemIndices '#' $ adj
        makeTree   = bd2 // [(co, '|')]
        openToTree = self == '.' && hasTrees
        hasTrees   = (>=3) . length . elemIndices '|' $ adj
        self = bd ! co
        adj  = map (bd !) $ getAdjacent bds co


getAdjacent :: (Coord, Coord) -> Coord -> [Coord]
getAdjacent bds (x,y) = [ (w,z)
                        | w <- [x-1..x+1]
                        , z <- [y-1..y+1]
                        , (w,z) /= (x,y)  
                        , w >= minX
                        , w <= maxX
                        , z >= minY
                        , z <= maxY]
  where 
    ((minX, minY), (maxX, maxY)) = bds

parse :: String -> PState
parse (lines -> xs) = PState board 0 coordList bounds
  where
    board     = UA.array bounds dList
    dList     = zip coordList charsOnly
    coordList = [ (x, y)
                | x <- [0..maxX]
                , y <- [0..maxY] ]
    charsOnly = concat xs
    bounds    = ((0,0), (maxX, maxY))
    maxX      = (length xs) - 1
    maxY      = (length $ xs !! 0) - 1


coordSort :: Coord -> Coord -> Ordering
coordSort (x1,y1) (x2,y2)
  | x1 > x2 = GT 
  | x1 < x2 = LT 
  | y1 > y2 = GT 
  | y1 < y2 = LT

breakEvery :: Int -> [a] -> [[a]]
breakEvery _ [] = []
breakEvery n xs
  | length xs <= n = [xs]
  | otherwise     = (take (n+1) xs):breakEvery n (drop (n+1) xs) 