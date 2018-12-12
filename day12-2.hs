import Data.List
import Data.Char
import Control.Applicative((<$>), (<*>))
import Control.Arrow ((&&&), (***))
import Debug.Trace

t' x = trace ("\n" ++ x ++ "\n") x

t'' str x = trace (str ++ (show . length $ x)) x


--- just watch output for diff to start repeating,  then apply this formula :

--((50000000000- <gen@repeating diff>)* <repeating diff amount>) + <result @ gen>

padNum = 4000 + 3 :: Int

main :: IO()
main = do
  input' <- readFile $ "./inputs/day12-1.txt"
  let (state:patterns) = tail . tail . words $ input'
  let paddedState = padding ++ state ++ padding
  cycleSolve (paddedState:patterns) 0 0 1
    where padding = take padNum $ repeat '.'

cycleSolve :: [String] -> Int -> Int -> Int -> IO ()
cycleSolve input@(state:patterns) lastGen lastTotal thisGen = do
  let (result, nextState) = solve thisGen input 
  let diff = result - lastTotal
  putStrLn $  "gen ="++(show thisGen) ++" result: " ++ (show result)  ++ " diff ="++ (show diff)
  if lastGen > 150 
    then return () 
    else cycleSolve input (lastGen + 1) result (thisGen + 1) 



solve :: Int -> [String] -> (Int, String)
solve n (initialState:patterns) 
  = (total, finalGen)
    where
      total = sum . map snd . filter ((=='#') . fst) $ indexed
      indexed = zip finalGen [negs..]
      finalGen = finalGen'
      finalGen' = solvePadding n len patterns $initialState
      negs = neg $ (padNum)
      neg n = n - n - n 
      len = length initialState



solvePadding :: Int -> Int-> [(String)] -> String-> String
solvePadding n len patterns state
  | n == 0    = state
  | otherwise = solvePadding (n-1) len patterns result 
  where 
    result = reverse . evolveState state $ ".."
    evolveState :: String-> String-> String
    evolveState []    acc 
      | length acc > len = evolveState [] (tail acc)
      | length acc < len = evolveState [] acc ++ "." 
      | otherwise        = acc 
    evolveState state acc
      | test `elem` patterns = evolveState (tail state) ('#':acc)
      | otherwise            = evolveState (tail state) ('.':acc)
      where test = take 5 $ state