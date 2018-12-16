{-# LANGUAGE ViewPatterns #-}
import Data.List
import Data.Char
import Data.Ord
import Control.Applicative((<$>), (<*>))
import Control.Arrow
import Data.Maybe
import Data.Function
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Array.Unboxed (UArray, (//), (!))
import qualified Data.Array.Unboxed as UA
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace


t' str x = trace ("\n"++ str ++ " " ++ (show x)) x
t'' x = traceShow x x

type Coord = (Int, Int)
type HP    = Int
type AP    = Int
type Board = UArray Coord Char
type Timer = Int
type Victory = Bool
type ElfDeath = Bool
-- Attacking Unit, Defending Unit
type Attack  = (Unit, Unit)
--distance, move this step, destination
type Move    = ((Int, Coord), Coord)

data UnitType  = Elf | Goblin deriving (Show, Eq, Ord)
data Unit      = Unit Coord UnitType HP AP deriving (Show, Eq, Ord)
-- Board, unmoved, moved, turn #
data GameState = GameState Board [Unit] [Unit] Timer Victory ElfDeath
  deriving (Eq, Ord)
instance Show GameState where
  show (GameState board unmoved moved timer v ed) =
    "\n turn: " ++ (show timer) ++ "\n" ++ boardChars ++ "\n moved: \n" ++ lmoved ++ "\n unmoved: \n" ++ lunmoved ++ "victory: " ++ (show v) ++ "\nelves died? " ++(show ed)
      where 
        (_, (h,w)) = UA.bounds board
        boardChars = unlines . breakEvery w . UA.elems $ board
        lmoved     = unlinez moved 
        lunmoved   = unlinez unmoved
        unlinez xs = unlines $ show <$> xs


-- now we need to find the first time where the elves win
-- extend parse to take a parameter
-- extend gameState to recognize elf death and quit game instantly

main :: IO()
main = do
  input <- readFile $ "./inputs/day15-1.txt"
  cycleSolve 4 input
  where 
    cycleSolve :: Int -> String -> IO ()
    cycleSolve n input = do
      let result = solve . parse n $ input 
      if getElfDeath result
        then 
          -- let statement = ("elves died with power: " ++ (show n)) 
          -- putStrLn $ statement
          cycleSolve (n + 1) input
        else 
          putStrLn $ (show result) ++ "\n power level: " ++ (show n) ++ "\nAnswer:" ++ (show . getAnswer $ result) 
          

solve :: GameState -> GameState
solve gs@(GameState _ unmoved moved t v ed)
  | getElfDeath gs = gs
  | hasVictory     = gs
  | null unmoved   = solve  $getNextRound gs
  | isNothing atk  = solve  newstate
  | isJust atk     = solve  resolved
  | otherwise      = error "the snozzberries taste like snozzberries"
  where 
    hasVictory      = v == True
    (newstate, atk) = moveUnit gs
    resolved = resolveCombat (newstate, fromJust atk)
--on each step, check for victory, check for combat, resolve combat, repeat

getAnswer :: GameState -> Int
getAnswer (GameState _ unmoved moved t v ed) 
  = (t *) . sum . map getHP $ unmoved ++ moved

getElfDeath :: GameState -> Bool
getElfDeath (GameState _ _ _ _ _ ed) = ed

getNextRound :: GameState -> GameState
getNextRound (GameState board [] moved t v ed)
  = GameState board nextUnmoved [] (t+1) v ed
    where
      nextUnmoved = sortBy sortUnits $ moved


resolveCombat :: (GameState, Attack) -> GameState
resolveCombat ((GameState board unmoved moved t v ed), 
  (attacker@(Unit _ _ _ aAP), defender@(Unit dcoord dut dHP _)))
    | hasDeath && dut == Elf = elfDeath
    | hasDeath               = death
    | otherwise              = hit
    where
      hasDeath = dHP <= aAP
      elfDeath = GameState dboard dunmoved dmoved t v True
      death    = GameState dboard dunmoved dmoved t v False
      hit      = GameState board hunmoved hmoved t v False
      dboard   = board // [(dcoord, '.')]
      dunmoved = filter (/=defender) unmoved
      dmoved   = filter (/=defender) moved
      hunmoved = map causeDamage unmoved
      hmoved   = map causeDamage moved
      causeDamage :: Unit -> Unit
      causeDamage victim@(Unit co ut _ ap)
        | victim == defender = Unit co ut (dHP - aAP) ap
        | otherwise          = victim



moveUnit :: GameState -> (GameState, Maybe Attack)
moveUnit gs@(GameState board (u@(Unit (x,y) ut hp ap):unmoved)  moved t v False)
  -- game is complete
  | victory         = ((GameState board unmoved (u:moved) t True False),  Nothing)
  -- there is an immediate attack available
  | immediateAttack = ((GameState board unmoved (u:moved) t False False), atk1) 
  | noMoves         = ((GameState board unmoved (u:moved) t False False), Nothing)
  -- move and check for secondary attack
  | secondAttack    = ((GameState nBoard unmoved (movedUnit:moved) t False False),  atk2)
  -- moveOnly 
  | otherwise       = ((GameState nBoard unmoved (movedUnit:moved) t False False),  Nothing) 
    where
      -- secondary attack helpers
      secondAttack   = not . null $ newTargets
      atk2           = Just (u, head newTargets)
      newTargets     = sortBy sortTargets . filter isInRange2 $ targets
      isInRange2     = (`elem` rangeAfterStep) . getCoords
      rangeAfterStep = getPossibleMoves step
      -- movement helpers
      movedUnit    = Unit step ut hp ap
      nBoard       = board //  [(step, toChar ut), ((x,y), '.')]
      ((_,step),_) = head movesByEnemy
      noMoves      = null movesByEnemy
      movesByEnemy = sortBy sortMoves . concat . map getMoves $ targets 
      getMoves     = (assessMvmt board allowedMoves . getCoords)
      allowedMoves = filter (( (== '.') . (board !))) $  moves
      -- immediate attack helpers
      immediateAttack = not . null $ immedTargets
      atk1            = Just (u, head immedTargets)
      immedTargets    = sortBy sortTargets . filter isInRange $ targets 
      isInRange       = (`elem` moves) . getCoords
      -- general
      victory  = length targets == 0
      moves    = getPossibleMoves (x,y)
      targets  = filter isEnemy $ allUnits
      isEnemy  = (==tt) . getUT
      allUnits = unmoved ++ moved
      tt       = getTargetType ut


sortTargets :: Unit -> Unit -> Ordering
sortTargets (Unit (x1,y1) _ h1 _) (Unit (x2,y2) _ h2 _)
  | h1 <  h2 = LT
  | h1 >  h2 = GT
  | x1 <  x2 = LT
  | x1 >  x2 = GT
  | y1 <  y2 = LT
  | y1 >  y2 = GT
  | otherwise = error "get to da choppa!"


-- take a list of squares in movement range and an enemy coordinate. determine if the enemy can be navigated to from each move, and if so, return the move
assessMvmt :: Board -> [Coord] -> Coord -> [Move]
assessMvmt board allowedmoves target = catMaybes . map getDistance' $ allowedmoves
  where getDistance' = getDistance board S.empty 1 (target)

-- move = (Distance, moveCoord, targetCoord)
-- collect the set of all neighbors and keep a counter for each iteration
-- if the set stops growing, stop & return Nothing
-- for each step, get all neighbors of each item in the set, filtered  by the if they are a member of set
-- check if one of the new items is the target
-- if target return just ((distance, origin) target)
-- otherwise, filter the new neighbors by whether or not they are '.' on the board.
-- add the filtered neighbors to the set and recurse
getDistance :: Board -> Set Coord -> Int -> Coord -> Coord -> Maybe Move
getDistance board pathSet distance ( target) (origin)
  | (board ! origin) /='.' = Nothing
  | pathSet == S.empty   = getDistance board (S.insert origin pathSet) distance target origin
  | target `elem` nextNeighbors = Just ((distance, origin), target)
  | pathSet == nextSet   =  Nothing
  | otherwise = getDistance board nextSet (distance+1) target origin
  where 
    ( nextSet)       = addListToSet pathSet $ filter ((=='.') . (board !)) $  nextNeighbors
    nextNeighbors = filter (not . (`S.member`pathSet)) . getPossibleMoves =<< (S.toList pathSet)


addListToSet :: (Ord a)=> Set a -> [a] ->  Set a
addListToSet acc []     = acc
addListToSet acc (x:xs) = addListToSet (S.insert x acc) xs

--sort by distance, then by destination coord reading order, then by move coord reading order.
sortMoves :: Move -> Move -> Ordering 
sortMoves ((d1, (mx1, my1)), (x1, y1)) ((d2, (mx2, my2)), (x2, y2))
  | d1 <  d2 = LT
  | d1 >  d2 = GT
  | x1 <  x2 = LT
  | x1 >  x2 = GT
  | y1 <  y2 = LT
  | y1 >  y2 = GT
  | mx1 <  mx2 = LT
  | mx1 >  mx2 = GT
  | my1 <  my2 = LT
  | my1 >  my2 = GT
  | otherwise = error "that's unpossible"

sortUnits :: Unit -> Unit -> Ordering
sortUnits (getCoords -> (x1,y1)) (getCoords -> (x2,y2))
  | x1 <  x2 = LT
  | x1 >  x2 = GT
  | y1 <  y2 = LT
  | y1 >  y2 = GT

getPossibleMoves :: Coord -> [Coord]
getPossibleMoves (x,y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]


parse:: Int -> String -> GameState
parse  n str   = GameState board units [] 0 False False
  where
    prepped    = lines str 
    h          = (length prepped) -1 
    w          = (length $ head prepped) -1
    dims       = ((0,0), (h,w))
    units      = fmap makeUnit . filter (isUnit . snd) $ coordChars
    isUnit  x  = x == 'G' || x == 'E'
    board      = UA.array dims coordChars
    coordChars = zip allCoords . concat $ prepped
    allCoords  = [ (x,y)
                 | x <- [0..h]
                 , y <- [0..w]
                 ]
    makeUnit :: (Coord, Char) -> Unit
    makeUnit (coord, c) 
      | c == 'G'  = Unit coord Goblin 200 3
      | c == 'E'  = Unit coord Elf    200 n
      | otherwise = error "Houston we have a problem"

breakEvery :: Int -> [a] -> [[a]]
breakEvery w [] = []
breakEvery w xs = (take (w+1) xs):breakEvery w (drop (w+1) xs)

getHP :: Unit -> HP
getHP (Unit _ _ hp _) = hp

getCoords :: Unit -> Coord
getCoords (Unit cs _ _ _) = cs

getUT :: Unit -> UnitType
getUT (Unit _ ut _ _) = ut

toChar :: UnitType -> Char
toChar Goblin = 'G'
toChar Elf    = 'E'

getTargetType :: UnitType -> UnitType
getTargetType x
  | x == Elf    = Goblin
  | x == Goblin = Elf