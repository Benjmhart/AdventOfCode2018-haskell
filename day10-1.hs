import Data.List
import Data.Char
import Control.Applicative((<$>), (<*>))

main :: IO()
main = do
  input <- readFile $ "./inputs/day10-1.txt"
  putStrLn $ solve $ input
  return ()

type Dims = (Int, Int, Int, Int)

data Point = Point { coords   :: (Int, Int) 
                   , velocity :: (Int, Int)
                   }deriving (Eq, Ord, Show)

solve = unlines . buildGridView . evolveList' . parse

-- when the height for a given iteration is within a single line of text, stop

evolveList' :: [Point] -> [Point]
evolveList'  points = evolveList (getDims points) points


evolveList :: Dims -> [Point] -> [Point]
evolveList dims points
  | h <= 10  = points
  | otherwise = evolveList nextDims nexts
  where (h, w)   = getHW dims
        nextDims = getDims nexts
        nexts    = iterateList points


parse :: String -> [Point]
parse = map pipeline . lines
  where pipeline = toPoint . map read . words . filter (not . (`elem` ['=', '>', '<', ','])) . filter ((||) <$> (not . isAlpha) <*> isSpace)
        toInt x = read x :: Int
        toPoint (a:b:c:d:_) = Point (a, b) (c, d)


iterateList :: [Point] -> [Point]
iterateList [] = []
iterateList ((Point (x,y) vs@(xv, yv)):xs) = (Point (x+xv, y+yv) vs): iterateList xs

buildGridView :: [Point] -> [String]
buildGridView points = transpose view
  where 
    dims@(l,r, t,b)  = getDims points
    (h, w)           = getHW dims
    view             = breakEvery h . map assignChar $ coordList 
    pointCoords      = map extractCoordPair $ points
    coordList        = [ (x,y) 
                       | x <- [l..r]
                       , y <- [t..b]
                       ]
    assignChar co
      | co `elem` pointCoords = '#'
      | otherwise             = '.'

breakEvery :: Int -> [a] -> [[a]]
breakEvery _ [] = []
breakEvery n xs
  | length xs <= n = [xs]
  | otherwise     = (take (n+1) xs):breakEvery n (drop (n+1) xs) 
    
getDims :: [Point] -> Dims
getDims points = (leftEdge, rightEdge, topEdge, bottomEdge)
  where
    xs            = map extractXCoord $ points
    ys            = map extractYCoord $ points
    leftEdge      = minimum xs
    rightEdge     = maximum xs
    topEdge       = minimum ys
    bottomEdge    = maximum ys


getHW :: Dims -> (Int,Int)
getHW (l, r, t, b) = (b - t, r - l)

extractCoordPair :: Point -> (Int,Int)
extractCoordPair (Point p _) = p  

extractXCoord :: Point -> Int
extractXCoord (Point (x, _) _) = x

extractYCoord :: Point -> Int
extractYCoord (Point (_, y) _) = y