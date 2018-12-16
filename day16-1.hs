import Data.List
import Data.Char
import Control.Applicative((<$>), (<*>))
import Data.Maybe
import Data.Bits
import Debug.Trace

t' str x = trace ("\n"++ str ++ " " ++ (show x)) x
t'' x = traceShow x x 

type Before = [Int]
type Code   = [Int]
type After  = [Int]

data OpCode = OpCode Before Code After [String] 
  deriving (Show, Eq, Ord)

-- get a list of all samples
-- run each of them through a check for each opcode.  if it matches, add it to the list in the opcode record.

-- at the end. filter out the opcodes with list length of < 3
-- take the length

main :: IO()
main = do
  input <- readFile $ "./inputs/day16-1a.txt"
  putStrLn $ show . solve . parse $ input
  return ()


solve :: [OpCode] -> Int
solve = length . filter (>=3) . map (length . t'' . getMatches . checkAll) 
  where
    checkAll = addrTest . addiTest . mulrTest . muliTest . banrTest . baniTest . borrTest . boriTest . setrTest . setiTest . gtirTest . gtriTest . gtrrTest . eqirTest . eqrrTest . eqriTest 




addrTest :: OpCode -> OpCode
addrTest oc@(OpCode before code after matches)
  | (addr before code) == after = OpCode before code after ("addr":matches)
  | otherwise                 = oc
  where
    addr:: [Int] -> [Int] -> [Int]
    addr reg (_: a: b: c: _) = replace c result reg
      where result           
              | ((length reg) - 1) < a = 111
              | ((length reg) - 1) < b = 111
              | otherwise              = (reg !! a) + (reg !! b)

addiTest :: OpCode -> OpCode
addiTest oc@(OpCode before code after matches)
  |  (addi before code) == after = OpCode before code after ("addi":matches)
  | otherwise                       = oc
  where
    addi:: [Int] -> [Int] -> [Int]
    addi reg (_: a: b: c: _) = replace c result reg
      where result           
              | ((length reg) - 1) < a = 111
              | otherwise              = (reg !! a) + b

mulrTest :: OpCode -> OpCode
mulrTest oc@(OpCode before code after matches)
  |  (mulr before code) == after = OpCode before code after ("mulr":matches)
  | otherwise                       = oc
  where
    mulr:: [Int] -> [Int] -> [Int]
    mulr reg (_: a: b: c: _) = replace c result reg
      where result           
              | ((length reg) - 1) < a = 111
              | ((length reg) - 1) < b = 111
              | otherwise              = (reg !! a) * (reg !! b)

muliTest :: OpCode -> OpCode
muliTest oc@(OpCode before code after matches)
  |  (muli before code) == after = OpCode before code after ("muli":matches)
  | otherwise                       = oc
  where
    muli:: [Int] -> [Int] -> [Int]
    muli reg (_: a: b: c: _) = replace c result reg
      where result           
              | ((length reg) - 1) < a = 111
              | otherwise              = (reg !! a) * b

banrTest :: OpCode -> OpCode
banrTest oc@(OpCode before code after matches)
  |  (banr before code) == after = OpCode before code after ("banr":matches)
  | otherwise                       = oc
  where
    banr:: [Int] -> [Int] -> [Int]
    banr reg (_: a: b: c: _) = replace c result reg
      where result           
              | ((length reg) - 1) < a = 111
              | ((length reg) - 1) < b = 111
              | otherwise              = (reg !! a) .&. (reg !! b)

baniTest :: OpCode -> OpCode
baniTest oc@(OpCode before code after matches)
  |  (bani before code) == after = OpCode before code after ("bani":matches)
  | otherwise                       = oc
  where
    bani:: [Int] -> [Int] -> [Int]
    bani reg (_: a: b: c: _) = replace c result reg
      where result           
              | ((length reg) - 1) < a = 111
              | otherwise              = (reg !! a) .&. b

borrTest :: OpCode -> OpCode
borrTest oc@(OpCode before code after matches)
  |  (borr before code) == after = OpCode before code after ("borr":matches)
  | otherwise                       = oc
  where
    borr:: [Int] -> [Int] -> [Int]
    borr reg (_: a: b: c: _) = replace c result reg
      where result           
              | ((length reg) - 1) < a = 111
              | ((length reg) - 1) < b = 111
              | otherwise              = (reg !! a) .|. (reg !! b)

boriTest :: OpCode -> OpCode
boriTest oc@(OpCode before code after matches)
  |  (bori before code) == after = OpCode before code after ("bori":matches)
  | otherwise                       = oc
  where
    bori:: [Int] -> [Int] -> [Int]
    bori reg (_: a: b: c: _) = replace c result reg
      where result           
              | ((length reg) - 1) < a = 111
              | otherwise              = (reg !! a) .|. b

setrTest :: OpCode -> OpCode
setrTest oc@(OpCode before code after matches)
  |  (setr before code) == after = OpCode before code after ("setr":matches)
  | otherwise                       = oc
  where
    setr:: [Int] -> [Int] -> [Int]
    setr reg (_: a: b: c: _) = replace c result reg
      where result           
              | ((length reg) - 1) < a = 111
              | otherwise              = (reg !! a) 

setiTest :: OpCode -> OpCode
setiTest oc@(OpCode before code after matches)
  |  (seti before code) == after = OpCode before code after ("seti":matches)
  | otherwise                       = oc
  where
    seti:: [Int] -> [Int] -> [Int]
    seti reg (_: a: b: c: _) = replace c result reg
      where result = a

gtirTest :: OpCode -> OpCode
gtirTest oc@(OpCode before code after matches)
  |  (gtir before code) == after = OpCode before code after ("gtir":matches)
  | otherwise                       = oc
  where
    gtir:: [Int] -> [Int] -> [Int]
    gtir reg (_: a: b: c: _) = replace c result reg
      where result           
              | ((length reg) - 1) < b = 111
              | a > (reg !! b) = 1
              | otherwise      = 0

gtriTest :: OpCode -> OpCode
gtriTest oc@(OpCode before code after matches)
  |  (gtri before code) == after = OpCode before code after ("gtri":matches)
  | otherwise                       = oc
  where
    gtri:: [Int] -> [Int] -> [Int]
    gtri reg (_: a: b: c: _) = replace c result reg
      where result           
              | ((length reg) - 1) < a = 111
              | (reg !! a) > b = 1
              | otherwise      = 0

gtrrTest :: OpCode -> OpCode
gtrrTest oc@(OpCode before code after matches)
  |  (gtrr before code) == after = OpCode before code after ("gtrr":matches)
  | otherwise                       = oc
  where
    gtrr:: [Int] -> [Int] -> [Int]
    gtrr reg (_: a: b: c: _) = replace c result reg
      where result           
              | ((length reg) - 1) < a = 111
              | ((length reg) - 1) < b = 111
              | (reg !! a) > (reg !! b) = 1
              | otherwise               = 0

eqirTest :: OpCode -> OpCode
eqirTest oc@(OpCode before code after matches)
  |  (eqir before code) == after = OpCode before code after ("eqir":matches)
  | otherwise                       = oc
  where
    eqir:: [Int] -> [Int] -> [Int]
    eqir reg (_: a: b: c: _) = replace c result reg
      where result           
              | ((length reg) - 1) < b = 111
              | a == (reg !! b) = 1
              | otherwise       = 0

eqriTest :: OpCode -> OpCode
eqriTest oc@(OpCode before code after matches)
  |  (eqri before code) == after = OpCode before code after ("eqri":matches)
  | otherwise                       = oc
  where
    eqri:: [Int] -> [Int] -> [Int]
    eqri reg (_: a: b: c: _) = replace c result reg
      where result           
              | ((length reg) - 1) < a = 111
              | (reg !! a) == b = 1
              | otherwise       = 0

eqrrTest :: OpCode -> OpCode
eqrrTest oc@(OpCode before code after matches)
  |  (eqrr before code) == after = OpCode before code after ("eqrr":matches)
  | otherwise                       = oc
  where
    eqrr:: [Int] -> [Int] -> [Int]
    eqrr reg (_: a: b: c: _) = replace c result reg
      where result           
              | ((length reg) - 1) < a = 111
              | ((length reg) - 1) < b = 111
              | (reg !! a) == (reg !! b) = 1
              | otherwise                = 0

parse :: String -> [OpCode]
parse input = map (makeOpCode . pipeline) . mergeThree . lines $ input
  where
    makeOpCode :: [Int] -> OpCode
    makeOpCode (a:b:c:d:e:f:g:h:i:j:k:l:_)
      = OpCode [a,b,c,d] [e,f,g,h] [i,j,k,l] []
    pipeline :: String -> [Int]
    pipeline  =  map read . words . filter makeItGood . map transformPunc
    transformPunc x
      | isPunctuation x = ' '
      | otherwise       = x
    makeItGood x = isSpace x || isDigit x 

mergeThree :: [String] -> [String]
mergeThree [] = []
mergeThree xs
  | length xs < 3 = xs
  | otherwise     = (concat . take 3 $ xs): mergeThree (drop 4 xs)


getBefore (OpCode before _ _ _) = before
getCode   (OpCode _ code   _ _) = code
getAfter  (OpCode _ _ after  _) = after
getMatches (OpCode _ _ _ match) = match

replace:: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace n b (a:as)
  | n == 0    = (b:as)
  | otherwise = a: (replace (n-1) b as)