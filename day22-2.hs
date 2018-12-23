{-# LANGUAGE ViewPatterns, TupleSections, DeriveGeneric #-}
import Data.Char
import Data.List
import Data.Maybe
import Data.Foldable
import Data.Monoid
import Data.Tuple
import Data.Function
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.Graph.AStar (aStar)
import qualified Data.HashSet as HS
import Control.Arrow ((&&&), (***))
import Control.Monad
import Control.Applicative ((<$>), (<*>))
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
type TerrainMap = Map Coord TileType
data TileType = Rocky | Wet | Narrow deriving (Eq, Ord, Show)
data Tile  = Tile Char (Maybe ErosionLevel) (Maybe GeoIndex) (Maybe TileType) deriving (Eq, Ord, Show)
data Cave  = Cave Board Depth LUTable [(Coord, Tile)] Coord
instance Show Cave where
  show (Cave bd depth tbl tiles t) = "Board:\n" ++  boardChars
    where 
      (_, (_,w)) = UA.bounds bd
      boardChars = unlines . transpose . breakEvery w $ boardString
      boardString = UA.elems $ bd
      
      -- unlinez xs = unlines $ show <$> xs

data Equip = CGear | Torch | Neither deriving (Eq, Ord, Show, Generic)
instance Hashable Equip

type Position = (Equip, Coord)

main :: IO()
main = do
  input <- readFile $ "./inputs/day22-2.txt"
  let cave = evaluateCave . parse $ input
  let caveMap = M.fromList $ (id *** fromJust . getTileType)<$> (getCaveTiles $ cave)
  putStr $ (++ "\n") . show $ cave 
  putStrLn $ show . solve caveMap $ cave
  return ()


--part 2 - make a map of 

solve :: Map Coord TileType -> Cave -> Maybe Int
solve mp (Cave _ _ _ _ t) = processRoute `liftM` route
  where
    processRoute = sum . t'' . map (uncurry getDistanceNeighbor) . (zip <*> tail) 
    route        = t'' $ getRoute mp t

getRoute :: TerrainMap -> Coord -> Maybe [Position]
getRoute tilemap target = (start :) <$> aStar 
                          (HS.fromList . getMoves tilemap) 
                          getDistanceNeighbor
                          (getDistanceToTarget end)
                          (==end)
                          start
  where 
    start = (Torch, (0,0))
    end   = (Torch, target)

getDistanceNeighbor :: Position -> Position -> Int
getDistanceNeighbor (e1, _) (e2,_)
  | e1 == e2  = 1
  | otherwise = 7

getDistanceToTarget :: Position -> Position -> Int
getDistanceToTarget (e1, co1) (e2, co2)
  | e1 == e2  = getMHDistance co1 co2
  | otherwise = (getMHDistance co1 co2) + 7

getMoves :: TerrainMap -> Position -> [Position]
getMoves tilemap (e, co@(x, y)) = filter (uncurry isCompatible) $ equipments ++ movements
  where
    equipments     = map (,co) . filter (/=e) $ [CGear, Torch, Neither]
    movements      = map (e,) $ neighbors co
    isCompatible equip coord = maybe False (`compatible` equip) $ (`M.lookup` tilemap) coord
  

compatible :: TileType -> Equip -> Bool
compatible Rocky  equip = equip == CGear || equip == Torch
compatible  Wet   equip = equip == CGear || equip == Neither
compatible Narrow equip = equip == Torch || equip == Neither


neighbors :: Coord -> [Coord]
neighbors (x, y) = [ (w,z)
                   | w <- [x-1..x+1]
                   , z <- [y-1..y+1]
                   , (w,z) /= (x,y) 
                   , w == x || z == y]


evaluateCave:: Cave -> Cave
evaluateCave (Cave bd depth tbl tiles targ) 
  = Cave  nextBd depth nextTbl (reverse nextTiles) targ
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
parse (pipeline -> (depth,target@(tx, ty))) 
  = Cave board depth tbl tiles target
  where 
    tbl            = M.fromList $ tiles
    board          = UA.array bnds $ map (id *** getTileChar) tiles
    bnds           = ((0,0), (tx+5, ty+5))
    tiles          = map (id &&& makeTile) allCoords
    allCoords      = [ (x,y) 
                   | x <- [0..tx+5]
                   , y <- [0..ty+5] 
                   
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

getCaveTiles (Cave _ _ _ tt _) = tt

getTileChar (Tile c _ _ _)  = c
getTileGeo  (Tile _ _ g _)  = g
getTileEro  (Tile _ e _ _)  = e
getTileType (Tile _ _ _ tt)  = tt

typeToInt :: TileType -> Int
typeToInt Rocky  = 0
typeToInt Wet    = 1
typeToInt Narrow = 2 

breakEvery :: Int -> [a] -> [[a]]
breakEvery _ [] = []
breakEvery n xs
  | length xs <= n = [xs]
  | otherwise     = (take (n+1) xs):breakEvery n (drop (n+1) xs)

getMHDistance :: Coord -> Coord -> Int
getMHDistance (x1, y1) (x2, y2) = distance
 where distance = (abs(x2 - x1)) + (abs(y2 - y1))