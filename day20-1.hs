{-# LANGUAGE TypeApplications, ViewPatterns #-}

import Data.List
import Data.Char
import Control.Applicative((<$>), (<*>))
import Data.Maybe

import Data.Graph.Inductive (UGr, level, mkUGraph)
import Data.Map (Map, (!))
import qualified Data.Map.Strict as M 
import Data.Maybe
import Debug.Trace

t str x = trace str x
t' str x = trace ( str ++ " " ++ (show x)) x
t'' x = traceShow x x 

type Distance = Int
type Coord = (Int, Int)
type DistanceMap = Map Coord Distance




main :: IO()
main = do
  input <- readFile $ "./inputs/day20-1.txt"
  let initial = parse $ input
  putStrLn $ show . solve $ initial
  return ()

solve :: UGr -> Int
solve graph = maximum . fmap snd . level 0 $ graph


dirChars = "NESW" :: String

test1 = "^NWWE$"
test2 = "^NE(W|EE)"
test3 = "^N(W|E)E"


parse :: String -> UGr 
parse input = mkUGraph (M.elems vertices) edges
  where 
    prepped = prep input
    prep = dropWhile (== '^') . dropWhileEnd (== '$') . filter (not . isSpace)
    (vertices, edges) = buildGraph [] (M.singleton @(Int, Int) (0, 0) 0) [] (0, 0) prepped
    buildGraph stack vs es p ('(':ds) = buildGraph (p:stack) vs es p ds
    buildGraph stack@(p:_) vs es _ ('|':ds) = buildGraph stack vs es p ds
    buildGraph (p:stack) vs es _ (')':ds) = buildGraph stack vs es p ds
    buildGraph stack vs es p@(x, y) (d:ds)
      | d `elem` "NESW" = buildGraph stack vs' ((vs ! p, v):es) p' ds
      where p' = case d of
                'N' -> (x, y - 1)
                'E' -> (x + 1, y)
                'S' -> (x, y + 1)
                'W' -> (x - 1, y)
                _ -> error $ "not a valid direction: " ++ [d]
            (fromMaybe (M.size vs) -> v, vs') =
                M.insertLookupWithKey (\_ _ a -> a) p' (M.size vs) vs
    buildGraph [] vs es _ "" = (vs, es)
    buildGraph _ _ _ _ _ = error "mismatched parentheses"