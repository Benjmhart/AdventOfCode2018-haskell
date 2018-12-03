import Data.Set(Set(..), insert, member, empty, size)
import Data.Char

type Claim = (Integer,Integer,Integer,Integer)
type Coordinate = (Integer,Integer)
type ClaimList = [Coordinate]

main :: IO()
main = do
  input <- readFile $ "./inputs/day3-1.txt"
  let strList = lines $ input
  putStr $ (++ "\n") . show . solve $ strList
  return ()

solve :: [String] -> Int
solve = size . makeSets' . concat . fmap (getCoordsList . prep)

makeSets' = makeSets empty empty

makeSets :: Set Coordinate -> Set Coordinate -> ClaimList ->  Set Coordinate
makeSets _       claimedTwice []     = claimedTwice 
makeSets claimed claimedTwice (x:xs) 
  | isClaimed2 = makeSets claimed     claimedTwice xs
  | isClaimed  = makeSets claimed     nextClaimed2 xs
  | otherwise  = makeSets nextClaimed claimedTwice xs
  where isClaimed    = x `member` claimed
        isClaimed2   = x `member` claimedTwice 
        nextClaimed  = insert x claimed
        nextClaimed2 = insert x claimedTwice
        

getCoordsList :: Claim -> ClaimList
getCoordsList (x,y,w,h) = [(x',y') | x' <- [x..re], y' <- [y..be], x >= 0, y >=0, x <=999, y<=999]
  where re = (x + w) - 1 --right edge
        be = (y + h) - 1 -- bottom edge

-- need a function that takes "#1 @ 1,3: 4x4: 4x4" and turns it into the list of tuples that represent the squares in that claim
prep :: String -> Claim
prep = toTuple . fmap read . tail . words . fmap convertForWords
  where toTuple (a:b:c:d:xs) = (a,b,c,d)
        toTuple xs           = error "rectum? damn near killed em!"

convertForWords :: Char -> Char
convertForWords x
  | isDigit x = x
  | x == ','  = ' '
  | x == '#'  = ' '
  | x == '@'  = ' '
  | x == 'x'  = ' '
  | x == ':'  = ' '
  | otherwise = x