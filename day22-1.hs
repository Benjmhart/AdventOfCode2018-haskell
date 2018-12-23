{-# LANGUAGE ViewPatterns #-}
import Data.Char
import Data.List
import Data.Maybe
import Data.Foldable
import Data.Monoid
import Control.Arrow ((&&&), (***))
import Control.Monad
import Data.Array.Unboxed (UArray, (//))
import qualified Data.Array.Unboxed as UA
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace

t str x = trace str x
t' str x = trace ( str ++ " " ++ (show x)) x
t'' x = traceShow x x

type Coord = (Int, Int)
type ErosionLevel = Int
type GeoIndex = Int
type Board = UArray (Int, Int) Char
type Depth = Int
type LUTable = Map Coord Tile 
data TileType = Rocky | Wet | Narrow deriving (Eq, Ord, Show)
data Tile  = Tile Char (Maybe ErosionLevel) (Maybe GeoIndex) (Maybe TileType) deriving (Eq, Ord, Show)
data Cave  = Cave Board Depth LUTable [(Coord, Tile)]
instance Show Cave where
  show (Cave bd depth tbl tiles) = "Board:\n" ++  boardChars
    where 
      (_, (_,w)) = UA.bounds bd
      boardChars = unlines . transpose . breakEvery w $ boardString
      boardString = UA.elems $ bd
      
      -- unlinez xs = unlines $ show <$> xs

main :: IO()
main = do
  input <- readFile $ "./inputs/day22-1.txt"
  putStr $ (++ "\n"). show . solve . evaluateCave . parse $ input
  return ()


solve :: Cave -> Sum Int
solve (Cave bd depth tbl tiles) 
  = foldMap (Sum . typeToInt . fromJust . getTileType . snd) tiles



evaluateCave:: Cave -> Cave
evaluateCave (Cave bd depth tbl tiles) = Cave  nextBd depth nextTbl (reverse nextTiles)
  where
    nextBd = bd // (map (id *** getTileChar) . reverse $ nextTiles)
    (nextTbl, nextTiles) = foldl correctTile (tbl, []) tiles
    correctTile:: (LUTable, [(Coord, Tile)]) -> (Coord,Tile) -> (LUTable, [(Coord, Tile)])
    correctTile (table, acc) ((x,y), (Tile char ero geo ty))
      = (nextTable, nextAcc) 
      where
        nextTable = M.insert (x,y) corrected table
        nextAcc   = (((x,y), corrected):acc)
        corrected = Tile c el gi tt 
        tableRef1 = getTileEro =<< (M.lookup (x-1,y) table)
        tableRef2 = getTileEro =<< (M.lookup (x,y-1) table)
        gi 
          | isJust geo = geo
          | otherwise  = liftM2 (*) tableRef1 tableRef2
        el
          | isJust ero = ero
          | otherwise  = (`mod` 20183) `liftM` ((+depth) `liftM` gi)
        elmod3         = (`mod`3) `liftM` el
        tt
          | isJust ty              = ty
          | isNothing elmod3       = Nothing
          | (fromJust elmod3) == 0 = Just Rocky
          | (fromJust elmod3) == 1 = Just Wet
          | (fromJust elmod3) == 2 = Just Narrow
        c 
         | char /= '?'       = char
         | tt == Nothing     = '?'
         | tt == Just Rocky  = '.'
         | tt == Just Wet    = '='
         | tt == Just Narrow = '|'



parse :: String -> Cave
parse (pipeline -> (depth,target@(tx, ty))) = Cave board depth tbl tiles
  where 
    tbl            = M.fromList $ tiles
    board          = UA.array bnds $ map (id *** getTileChar) tiles
    bnds           = ((0,0),target) --(tx+5, ty+5))
    tiles          = map (id &&& makeTile) allCoords
    allCoords      = [ (x,y) 
                   | x <- [0..tx] -- +5]
                   , y <- [0..ty] -- +5] 
                   
                   ]
    makeTile :: Coord -> Tile
    makeTile (x,y) = Tile ch el gi tt
      where
        gi 
          | (x,y) == (0,0) || (x,y) == (tx,ty) = Just 0
          | x == 0    = Just (y * 48271)
          | y == 0    = Just (x * 16807)
          | otherwise = Nothing
        el 
          | isJust gi = Just (((fromJust gi) + depth) `mod` 20183)
          | otherwise = Nothing
        tt
          | isNothing el               = Nothing
          | (fromJust el) `mod` 3 == 0 = Just Rocky 
          | (fromJust el) `mod` 3 == 1 = Just Wet
          | (fromJust el) `mod` 3 == 2 = Just Narrow  
        ch
          | (x,y) == (0,0)          = 'M'
          | (x,y) == target         = 'T'
          | isNothing tt            = '?'
          | (fromJust tt) == Rocky  = '.' 
          | (fromJust tt) == Wet    = '='
          | (fromJust tt) == Narrow = '|'  




pipeline = processBoth . secondFourth .  words 
  where 
    processBoth :: (String, String) -> (Int, Coord)
    processBoth =  read *** processRight
    processRight :: String -> (Int, Int)
    processRight = (read *** read . tail) . (break (==','))



secondFourth :: [a] -> (a, a)
secondFourth(_:c:_:d:_) = (c,d) 

getTileChar (Tile c _ _ _)  = c
getTileGeo  (Tile _ _ g _)  = g
getTileEro  (Tile _ e _ _)  = e
getTileType (Tile _ _ _ t)  = t

typeToInt :: TileType -> Int
typeToInt Rocky  = 0
typeToInt Wet    = 1
typeToInt Narrow = 2 

breakEvery :: Int -> [a] -> [[a]]
breakEvery _ [] = []
breakEvery n xs
  | length xs <= n = [xs]
  | otherwise     = (take (n+1) xs):breakEvery n (drop (n+1) xs) 