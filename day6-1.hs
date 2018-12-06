import Data.List
import Data.Char
import qualified Data.Set as S

main :: IO()
main = do
  input <- readFile $ "./inputs/day6-1.txt"
  putStr $ (++ "\n"). show . solveCount . solveBoard . parse $ input
  return ()

type UnsignedCoord = (Int, Int)
type SignedCoord = (Char, UnsignedCoord)
type Dims  = (Int, Int, Int, Int)
type UnsolvedBoard = [UnsignedCoord]
type SolvedBoard = [SignedCoord]

solveCount :: (S.Set Char, SolvedBoard) -> Int
solveCount (edgeChars, solvedBoard) = last candidateList
  where candidateList = sort . map length . group . sort . map stripToFst . filter isNotEdge $  solvedBoard
        isNotEdge (c, _)
          | c `S.member` edgeChars = False
          | c == '.'               = False
          | otherwise              = True 
        stripToFst (a,_) = a 

solveBoard :: (Dims, [SignedCoord], UnsolvedBoard) -> (S.Set Char, SolvedBoard)
solveBoard ((l,r,t,b), coords, board) = (edgeChars, solvedBoard)
  where solvedBoard = map solveSquare $ board
        solveSquare (x,y) = removeSnd . head . checkForTie . sortBy compareSecond . map (getMHDistance (x, y)) $ coords
        edgeChars = getEdgeChars (l,r,t,b) solvedBoard S.empty
        checkForTie ((c1,d1,coord1):second@(c2, d2,coord2):xs)
          | d1 == d2  = ('.', d1, coord1):second:xs
          | otherwise = (c1, d1, coord1):second:xs

parse :: String -> (Dims, [SignedCoord], UnsolvedBoard)
parse xs = (dims, zipped, board)
  where zipped = zip ['a'..'z'] piped
        piped = map pipeline . lines $ xs
        pipeline = toPair . map toInt . words . isNotPunctuation 
        isNotPunctuation = filter (not . isPunctuation)
        toInt x = read x :: Int
        toPair (a:b:[]) = (a, b)
        dims = (maxLeft, maxRight, maxTop, maxBottom)
        maxLeft    = minimum . map fst $ piped
        maxTop     = minimum . map snd $ piped
        maxRight   = maximum . map fst $ piped
        maxBottom  = maximum . map snd $ piped
        board = [ (x, y) 
                | x <- [(maxLeft-1)..(maxRight+1)]
                , y <- [(maxTop-1)..(maxBottom+1)]]


getEdgeChars :: Dims -> SolvedBoard -> S.Set Char ->S.Set Char
getEdgeChars _ [] edgeChars = edgeChars
getEdgeChars dims@(l, r, t, b) ((c,(x,y)):xs) edgeChars
  | edge      = getEdgeChars dims xs nextEdgeChars
  | otherwise = getEdgeChars dims xs edgeChars  
  where
    nextEdgeChars = S.insert c edgeChars
    edge = x == (l - 1) || (x == (r + 1) || ( y == (t - 1) || y == (b + 1)))
  

removeSnd :: (a,b,c) -> (a, c)
removeSnd (a,_,c) = (a, c)

compareSecond :: (a, Int, c) -> (a, Int, c) -> Ordering
compareSecond (_, x, _) (_, y, _)
  | x >  y = GT
  | x <  y = LT
  | x == y = EQ 

getMHDistance :: UnsignedCoord -> SignedCoord -> (Char, Int, UnsignedCoord)
getMHDistance (x1, y1) (c, (x2, y2)) = (c, distance, (x1, y1))
 where distance = (abs(x2 - x1)) + (abs(y2 - y1))

