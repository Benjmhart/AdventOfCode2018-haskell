import Data.List


main :: IO()
main = do
  input <- readFile $ "./inputs/day8-1.txt"
  putStr $ (++ "\n"). show . solve $ input
  return ()

data Tree = Tree [Tree] [Int] deriving (Show, Eq)


solve :: String -> Integer
solve = getTreeSum . snd . makeChild . map read . words

getTreeSum :: Tree -> Integer
getTreeSum (Tree children ns) 
  | length children == 0 = dataSum
  | otherwise            = dataSum + childSum
  where 
    dataSum  = toInteger . sum $ ns
    childSum = sum . map getTreeSum $ children   

makeChild :: [Int] -> ([Int], Tree)
makeChild (cl:dl:rest) = (remaining, Tree children metadata)
  where
    remaining             = drop dl restAfter
    metadata              = take dl restAfter 
    (restAfter, children) = makeChildren cl (rest, [])

makeChildren :: Int -> ([Int], [Tree]) -> ([Int], [Tree])
makeChildren n (xs, trees)
  | n == length trees       = (xs, trees)
  | otherwise               = makeChildren n (nextXs, (nextChild:trees))
  where (nextXs, nextChild) = makeChild xs
