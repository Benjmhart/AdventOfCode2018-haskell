
import Data.List
import Data.Maybe

main :: IO()
main =  do
  input <- readFile $ "./inputs/day2-1.txt"
  let strList = words $ input
  putStr . (++ "\n") . checkForMatchOffByOne $ strList
  return ()


checkForMatchOffByOne :: [String] -> String
checkForMatchOffByOne xss = check x xs
  where (x:xs) = sort xss
        check _    [] = error "you done fucked up"
        check word (ys:yss)
          | isJust matches = fromJust matches
          | otherwise      = check ys yss
          where matches =  hasOneDifference word ys

hasOneDifference :: String -> String -> Maybe String
hasOneDifference xs ys
  | targetLength == diffLength = Just diff
  | otherwise                  = Nothing
  where targetLength = (length xs) -1
        diffLength = length diff
        diff = catMaybes . differ xs ys $ []

differ:: String -> String -> [Maybe Char] -> [Maybe Char]
differ []     []     ms = reverse ms
differ (x:xs) []     _  = []
differ []     (y:ys) _  = []
differ (x:xs) (y:ys) ms
  | x == y    = differ xs ys $ (Just x):ms
  | otherwise = differ xs ys $ Nothing :ms