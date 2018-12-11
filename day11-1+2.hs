import Control.Arrow
import Data.Array.Unboxed
import Data.List (maximumBy)
import Data.Ord
import Control.Applicative

type Coord = (Int, Int)

part1 :: Int -> String
part1 serial= show . fst . (!! 2) . maxScores . getScore serial $ allCoords 0

part2 :: Int -> String
part2 serial = show . extractTuple . maximumBy (comparing (snd . fst)) . (`zip` [1..300])
        . maxScores . getScore serial $ allCoords 0
getScore :: Int -> [Coord ]-> UArray Coord Int
getScore n coords = array ((1, 1), (300, 300)) $ (id &&& score n) <$> coords

allCoords :: Int -> [Coord]
allCoords n = [ (x,y)
              | x <- [1..300-n]
              , y <- [1..300-n]
              ]

score :: Int -> Coord -> Int
score n (x, y) = (`minus` 5) . (`mod` 10 ) . (`div` 100) . (* rid) . (+ n) . (* y) $ rid  
  where 
    rid = x + 10
    minus a b = a - b


maxScores :: UArray Coord Int -> [(Coord, Int)]
maxScores scoreTable = (maximumBy (comparing snd) . assocs) <$> scoreTable : getSquares 1 (const 0) (scoreTable !)
    where 
      getSquares :: Int -> (Coord -> Int) -> (Coord -> Int) -> [UArray Coord Int]
      getSquares size getScoreCorners getSquareScore = nextGrid : getSquares (size+1) getSquareScore (nextGrid !)
        where 
          nextGrid = array ((1,1), (300-size, 300 - size)) $ (id &&& f) <$> coordsThisIteration
          coordsThisIteration = allCoords size
          f (x, y) = getSquareScore (x, y) + getSquareScore (x+1, y+1)
                     + scoreTable ! (x, y+size) + scoreTable ! (x+size, y) - getScoreCorners (x+1,y+1)


extractTuple :: (((Int, Int), Int), Int) -> (Int, Int, Int)
extractTuple (((x, y), _), size) = (x, y, size)

