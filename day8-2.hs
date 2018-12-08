import Data.List

main :: IO()
main = do
  input <- readFile $ "./inputs/day8-1.txt"
  putStr $ (++ "\n"). show . solve $ input
  return ()

data Tree = Tree [Tree] [Int] deriving (Show, Eq)

solve :: String -> Integer
solve = getTreeValue . snd . makeChild . map read . words

getTreeValue :: Tree -> Integer
getTreeValue (Tree children ns) 
  | null children = dataSum
  | otherwise = sum dataChildCrossRef
  where 
    dataSum           = toInteger . sum $ ns
    childrenLength    = length children
    dataChildCrossRef = map crossRef ns
    crossRef x
      | childrenLength < x = toInteger 0
      | otherwise          = toInteger $ childValues !! (x-1)
    childValues            = map getTreeValue $ children   

makeChild :: [Int] -> ([Int], Tree)
makeChild (cl:dl:rest) = (remaining, Tree children metadata)
  where
    remaining              = drop dl restAfter
    metadata               = take dl restAfter 
    (restAfter, children)  = makeChildren cl (rest, [])

makeChildren :: Int -> ([Int], [Tree]) -> ([Int], [Tree])
makeChildren n (xs, trees)
  | n == length trees       = (xs, (reverse trees))
  | otherwise               = makeChildren n (nextXs, (nextChild:trees))
  where (nextXs, nextChild) = makeChild xs
