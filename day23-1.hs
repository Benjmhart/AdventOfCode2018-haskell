{-# LANGUAGE ViewPatterns #-}
import Data.Char
import Data.Ord
import Data.List
import Data.List.Split
import Debug.Trace

t str x = trace str x
t' str x = trace ( str ++ " " ++ (show x)) x
t'' x = traceShow x x
takeTrace xs = traceShow (take 5 xs) xs

type Coord = (Integer, Integer, Integer)
type Distance = Integer
data Nano = Nano Coord Distance deriving (Eq, Ord)
instance Show Nano where
  show (Nano co d) = "coordinates: "++ show co ++ " range: " ++ show d

main :: IO()
main = do
  input <- readFile $ "./inputs/day23-1.txt"
  let parsed = parse $ input
  -- putStrLn $ unlines . map show $ parsed
  putStrLn $ show . solve $ parsed
  return ()

solve :: [Nano] -> Int
solve ns = length . filter isInRange $ ns
  where
    leader         = head . reverse . sortBy (comparing getNanoRange) $ ns
    isInRange nano = (getMHDistance leader nano) <= (getNanoRange leader) 

getMHDistance :: Nano -> Nano -> Distance
getMHDistance (getNanoCoords ->(x1,y1,z1)) (getNanoCoords -> (x2,y2,z2)) 
  =  (abs(x2 - x1)) + (abs(y2 - y1)) + (abs(z2 - z1))

getNanoRange :: Nano -> Distance
getNanoRange (Nano _ d) = d 

getNanoCoords :: Nano-> Coord
getNanoCoords (Nano c _) = c

parse :: String -> [Nano]
parse xs = makeNano <$> pipeline xs
  where
    pipeline = map (map getCoords . words . filter keepStuff) . lines
    getCoords :: String -> [Integer]
    getCoords =  map read . filter (not . null) . (splitOn ",")
    keepStuff x = isDigit x || x == ',' || x == ' '
    makeNano :: [[Integer]] -> Nano
    makeNano ((x:y:z:_):(r:_):_) = Nano (x,y,z) r