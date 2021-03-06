import Data.Char

main :: IO()
main = do
  input <- readFile $ "./inputs/day6-1.txt"
  putStr $ (++ "\n"). show . solve $ input
  return ()

type Coord = (Int, Int)

maxDist :: Int
maxDist = 10000

solve :: String -> Int
solve xs = length [ (x,y) 
               | x <- [maxL..maxR] 
               , y <- [maxT..maxB ]
               , (< maxDist) . sum . map (getMHDistance (x,y)) $ origins 
               ]
  where origins = parse xs
        maxL = minimum . map fst $ origins
        maxT = minimum . map snd $ origins
        maxR = maximum . map fst $ origins
        maxB = maximum . map snd $ origins

parse :: String -> [Coord]
parse xs = origins
 where
  origins = map pipeline . lines $ xs
  pipeline = toPair . map read . words . isNotPunctuation 
  isNotPunctuation = filter (not . isPunctuation)
  toPair (a:b:[]) = (a, b)

getMHDistance :: Coord -> Coord -> Int
getMHDistance (x1, y1)  (x2, y2) = distance
 where distance = (abs(x1 - x2)) + (abs(y1 - y2))

