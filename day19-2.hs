{-# LANGUAGE ViewPatterns #-}

import Data.List
import Data.Char
import Control.Applicative((<$>), (<*>))
import Data.Maybe
import Data.Bits
import Debug.Trace
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM 

tr str x = trace str x
t' str x = trace (str ++ " " ++ (show x)) x
t'' x = traceShow x x 

data Func = ADDR | ADDI | MULR | MULI | BORR | BORI | BANR | BANI | SETR|SETI | GTIR | GTRR | GTRI | EQRI | EQIR | EQRR deriving (Eq, Ord, Show)

type Registers = [Int]

type Pointer = Int
type Time    = Int
type InstructionMap = IntMap Instruction

data PState = PS Registers Pointer Time deriving (Eq, Ord, Show)

data Instruction = Inst Func Int Int Int deriving (Eq, Ord, Show)

main :: IO()
main = do
  let solution = sum $ divisors 10551275
  putStrLn $ show solution
  return ()

divisors n = [ x | x <- [1..n], n `rem` x == 0]


---run the first 100 steps and see when the program enters the loop, take the large number at the beginning of the loop and find the sum of the divisiors

solve :: (PState, InstructionMap) -> (PState, InstructionMap)
solve args@((PS reg p t), is)
  | t > 100 = args
  | IM.size is <= (reg !! p) = args
  | otherwise = solve $ stepProgram args

stepProgram :: (PState, InstructionMap) -> (PState, InstructionMap)
stepProgram (ps@(PS reg p t), is) = (newPs, is)
  where 
    newPs       = updatePointer psBefPtrUpd
    psBefPtrUpd = (PS newReg p t)
    newReg      = executeStep reg
    executeStep :: Registers -> Registers
    executeStep rs 
      | func == ADDR = addr rs [a1, a2, a3]
      | func == ADDI = addi rs [a1, a2, a3]
      | func == MULR = mulr rs [a1, a2, a3]
      | func == MULI = muli rs [a1, a2, a3]
      | func == BORR = borr rs [a1, a2, a3]
      | func == BORI = bori rs [a1, a2, a3]
      | func == BANR = banr rs [a1, a2, a3]
      | func == BANI = bani rs [a1, a2, a3]
      | func == SETR = setr rs [a1, a2, a3]
      | func == SETI = seti rs [a1, a2, a3]
      | func == GTIR = gtir rs [a1, a2, a3]
      | func == GTRR = gtrr rs [a1, a2, a3]
      | func == GTRI = gtri rs [a1, a2, a3]
      | func == EQRI = eqri rs [a1, a2, a3]
      | func == EQIR = eqir rs [a1, a2, a3]
      | func == EQRR = eqrr rs [a1, a2, a3]
      where
        (Inst func a1 a2 a3) = t' ((show instNum)++ " " ++(show reg) ) $ fromJust $ IM.lookup instNum is
        instNum = (rs !! p)

updatePointer :: PState -> PState
updatePointer (PS reg p t) = (PS newReg p (t+1))
  where 
    newReg = replace p nextPointer reg
    nextPointer = (reg !! p) + 1

parse :: String -> (PState, InstructionMap)
parse (lines -> xs) = ((PS [1,0,0,0,0,0] p 0), is)
  where
    p  = read . (:[]) . last . head $ xs 
    is = IM.fromList . zip [0..] . map makeInstruction . tail $ xs
    makeInstruction :: String -> Instruction
    makeInstruction  str = Inst func a1 a2 a3
      where
        (a1:a2:a3:_)   = map read args
        (funcStr:args) = words str
        func = makeFunc funcStr
        makeFunc :: String -> Func
        makeFunc y 
          | y == "addr" = ADDR
          | y == "addi" = ADDI
          | y == "mulr" = MULR
          | y == "muli" = MULI
          | y == "borr" = BORR 
          | y == "bori" = BORI 
          | y == "banr" = BANR 
          | y == "bani" = BANI 
          | y == "setr" = SETR
          | y == "seti" = SETI 
          | y == "gtir" = GTIR 
          | y == "gtrr" = GTRR 
          | y == "gtri" = GTRI 
          | y == "eqri" = EQRI 
          | y == "eqir" = EQIR 
          | y == "eqrr" = EQRR

addr:: [Int] -> [Int] -> [Int]
addr reg (a: b: c: _) = replace c result reg
  where result = (reg !! a) + (reg !! b)

addi:: [Int] -> [Int] -> [Int]
addi reg (a: b: c: _) = replace c result reg
  where result = (reg !! a) + b

mulr:: [Int] -> [Int] -> [Int]
mulr reg (a: b: c: _) = replace c result reg
  where result = (reg !! a) * (reg !! b)

muli:: [Int] -> [Int] -> [Int]
muli reg (a: b: c: _) = replace c result reg
  where result = (reg !! a) * b

banr:: [Int] -> [Int] -> [Int]
banr reg (a: b: c: _) = replace c result reg
  where result = (reg !! a) .&. (reg !! b)

bani:: [Int] -> [Int] -> [Int]
bani reg (a: b: c: _) = replace c result reg
  where result = (reg !! a) .&. b

borr:: [Int] -> [Int] -> [Int]
borr reg (a: b: c: _) = replace c result reg
  where result = (reg !! a) .|. (reg !! b)

bori:: [Int] -> [Int] -> [Int]
bori reg (a: b: c: _) = replace c result reg
  where result = (reg !! a) .|. b


setr:: [Int] -> [Int] -> [Int]
setr reg (a: b: c: _) = replace c result reg
  where result = (reg !! a) 

seti:: [Int] -> [Int] -> [Int]
seti reg (a: b: c: _) = replace c result reg
  where result = a

gtir:: [Int] -> [Int] -> [Int]
gtir reg (a: b: c: _) = replace c result reg
  where result           
          | a > (reg !! b) = 1
          | otherwise      = 0

gtri:: [Int] -> [Int] -> [Int]
gtri reg (a: b: c: _) = replace c result reg
  where result           
          | (reg !! a) > b = 1
          | otherwise      = 0

gtrr:: [Int] -> [Int] -> [Int]
gtrr reg (a: b: c: _) = replace c result reg
  where result
          | (reg !! a) > (reg !! b) = 1
          | otherwise               = 0

eqir:: [Int] -> [Int] -> [Int]
eqir reg (a: b: c: _) = replace c result reg
  where result
          | a == (reg !! b) = 1
          | otherwise       = 0

eqri:: [Int] -> [Int] -> [Int]
eqri reg (a: b: c: _) = replace c result reg
  where result           
          | (reg !! a) == b = 1
          | otherwise       = 0

eqrr:: [Int] -> [Int] -> [Int]
eqrr reg (a: b: c: _) = replace c result reg
  where result           
          | (reg !! a) == (reg !! b) = 1
          | otherwise                = 0

replace:: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace n b (a:as)
  | n == 0    = (b:as)
  | otherwise = a: (replace (n-1) b as)