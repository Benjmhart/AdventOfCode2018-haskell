
import Data.Char
import System.IO (readFile)
import Data.Text (split, splitOn, pack, unpack)
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Array (Array)
import qualified Data.Array as A
import Control.Applicative

main :: IO ()
main = do
    input <- S.fromList . foldl1 (++) . map parseInput . lines <$> readFile "./inputs/day17-1.txt"

    print $ problem1 input
    print $ problem2 input

problem1 input = 
    let (minY, maxY) = ( minimum . S.toList . S.map snd $ input
                       , maximum . S.toList . S.map snd $ input)
    in
        -- take 10 . sortBy (\a b -> compare (snd a) (snd b)) . S.toList $ input
        S.size $ flow (minY, maxY) input S.empty [(500, minY)]


problem2 input = 
    let (minY, maxY) = ( minimum . S.toList . S.map snd $ input
                       , maximum . S.toList . S.map snd $ input)
    in
        -- take 10 . sortBy (\a b -> compare (snd a) (snd b)) . S.toList $ input
        S.size $ flow2 (minY, maxY) input S.empty [(500, minY)]

flow :: (Int, Int) -> Set (Int, Int) -> Set (Int, Int) -> [(Int, Int)] -> Set (Int, Int)
flow _ _ wet [] = wet
flow (minY, maxY) clay wet ((x,y):water)
    | y < minY || y > maxY = flow (minY, maxY) clay wet water
    | null $ exits' =
        flow (minY, maxY) (S.union clay newWet) (S.union wet newWet) ((x,y-1) : water)
    | all (flip S.member wet) $ exits' = flow (minY, maxY) clay (S.union wet newWet ) water
    | otherwise =
        flow (minY, maxY) clay (S.union wet newWet) (exits clay expands ++ water)
    where
        expands = expand clay (x,y)
        newWet = S.fromList expands
        exits' = exits clay expands

flow2 :: (Int, Int) -> Set (Int, Int) -> Set (Int, Int) -> [(Int, Int)] -> Set (Int, Int)
flow2 _ clay wet [] = S.intersection clay wet
flow2 (minY, maxY) clay wet ((x,y):water)
    | y < minY || y > maxY = flow2 (minY, maxY) clay wet water
    | null $ exits' =
        flow2 (minY, maxY) (S.union clay newWet) (S.union wet newWet) ((x,y-1) : water)
    | all (flip S.member wet) $ exits' = flow2 (minY, maxY) clay (S.union wet newWet ) water
    | otherwise =
        flow2 (minY, maxY) clay (S.union wet newWet) (exits clay expands ++ water)
    where
        expands = expand clay (x,y)
        newWet = S.fromList expands
        exits' = exits clay expands


exits :: Set (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
exits clay xs = filter (flip S.notMember clay) . map (\(x,y) -> (x,y+1)) $ xs
    
expand :: Set (Int, Int) -> (Int, Int) -> [(Int, Int)]
expand clay (x,y)
    | (x, y+1) `S.notMember` clay = [(x,y)]
    | otherwise = nub $ (expandL (x,y) ++ expandR (x,y))
    where
        expandL (x,y)
            | (x,y) `S.member` clay = []
            | (x, y+1) `S.notMember` clay = [(x, y)]
            | otherwise = (x,y) : expandL (x-1, y)
        expandR (x,y)
            | (x,y) `S.member` clay = []
            | (x, y+1) `S.notMember` clay = [(x, y)]
            | otherwise = (x,y) : expandR (x+1, y)

-- parseInput :: String -> [(Int, Int)]
parseInput input = mymap . map unpack . split ((flip elem) ", ") . pack $ input
    where
        mymap = (\[_, x, y] -> [(i,j) | i <- toList' (drop 2 x), j<- toList' (drop 2 y)])
                . sort

        toList' :: String -> [Int]
        toList' xs
            | '.' `elem` xs = [(read . takeWhile (/='.') $ xs)..
                               (read . reverse. takeWhile (/='.') . reverse $ xs)]
            | otherwise = [read xs]