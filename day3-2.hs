import Data.Set(Set(..), insert, member, empty, size)
import Data.List(delete)
import Data.Char
import Data.Maybe(isJust)

testString ="#1 @ 1,3: 4x4"

type ID = Integer
type Claim = (ID, Integer,Integer,Integer,Integer)
type Coordinate = (Integer,Integer)
type ClaimList = (ID, [Coordinate])

main :: IO()
main = do
  input <- readFile $ "./inputs/day3-1.txt"
  let strList = lines $ input
  putStr $ (++ "\n") . show . solve $ strList
  return ()

solve :: [String] -> ID
solve strings = findClaim' claims
  where claims     = fmap (getCoordsList . prep) strings
        findClaim' [] = (-1)
        findClaim' (claim@(id, _):cs)
          | isJust (findClaim claim) = id
          | otherwise              = findClaim' cs
          where claimedSet = checkClaim' . concat . fmap snd . delete claim $ claims
                findClaim :: ClaimList -> Maybe ID
                findClaim (id, []) = Just id
                findClaim (id, (y:ys))
                  | claimed = Nothing
                  | otherwise = findClaim (id, ys)
                  where claimed =  y `member` claimedSet
          


checkClaim' :: [Coordinate] ->  Set Coordinate
checkClaim' = checkClaim empty

checkClaim :: Set Coordinate -> [Coordinate] ->  Set Coordinate
checkClaim claimed []    = claimed
checkClaim claimed (x:xs)
  | isClaimed = checkClaim claimed xs
  | otherwise = checkClaim nextClaimed xs
  where isClaimed    =  x `member` claimed
        nextClaimed  = insert x claimed
        

getCoordsList :: Claim -> ClaimList
getCoordsList (id,x,y,w,h) = (id, [(x',y') | x' <- [x..re], y' <- [y..be], x >= 0, y >=0, x <=999, y<=999])
  where re = (x + w) - 1 --right edge
        be = (y + h) - 1 -- bottom edge

-- need a function that takes "#1 @ 1,3: 4x4: 4x4" and turns it into the list of tuples that represent the squares in that claim
prep :: String -> Claim
prep = toTuple . fmap read . words . fmap convertForWords
  where toTuple (a:b:c:d:e:xs) = (a,b,c,d,e)
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