import System.Environment
import Text.Printf
import Data.Foldable (toList)
import Text.Megaparsec (anySingle, satisfy, many, parse, parseErrorTextPretty, Parsec, eof)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Error (errorBundlePretty)
import Data.Void
import Data.List
import qualified Data.Set as Set
import           Data.Bits            ((.&.), (.|.))
import           Data.IntMap          (IntMap)
import qualified Data.IntMap.Strict   as IntMap
import           Data.Map             (Map)
import           Data.IntSet          (IntSet)
import qualified Data.Map             as Map
import qualified Data.IntSet          as IntSet
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vector
import           Text.Megaparsec      (endBy, eof, some)
import           Text.Megaparsec.Char (letterChar, newline, space)
import Data.List (findIndex)

type Registers = IntMap Int

-- | Print the answers to day 21
main :: IO ()
main =
  do (ip, pgm) <- getParsedInput 21 parseInput
     let regs = IntMap.fromList [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0)]
     let xs   = run ip pgm regs
     print (head xs)
     print (findCycle xs)

-- | Parse the input as an instruction pointer register and a vector
-- of register updating functions.
parseInput :: Parser (Int, Vector (Registers -> Registers))
parseInput =
  do ip  <- "#ip" *> space *> number <* newline
     pgm <- endBy parseInstruction newline <* eof
     pure (ip, Vector.fromList pgm)

findCycle :: [Int] -> Int
findCycle = go 0 IntSet.empty
  where
    go answer seen (x:xs)
      | IntSet.member x seen = answer
      | otherwise = go x (IntSet.insert x seen) xs

-- | Parse a register updating function
parseInstruction :: Parser (Registers -> Registers)
parseInstruction =
  do name <- some letterChar
     let operand = space *> number
     case Map.lookup name opcodes of
       Just f  -> f <$> operand <*> operand <*> operand
       Nothing -> fail "unknown instruction"

-- | Given a program counter register and a program, run the program
-- until the instruction pointer points outside of the program. The
-- final state of the registers is returned.
run :: Int -> Vector (Registers -> Registers) -> Registers -> [Int]
run ip pgm regs =
  case pgm Vector.!? pc of
    Nothing -> []
    Just f
      | pc == 29 -> regs IntMap.! 5 : run ip pgm (nextIP (f regs))
      | otherwise -> run ip pgm (nextIP (f regs))
  where
    pc = regs IntMap.! ip
    nextIP = IntMap.update (Just . (1+)) ip

-- | Map from opcode names to opcode semantics. The functions expect
-- the operands A, B, and C as well as the current registers.
opcodes :: Map String (Int -> Int -> Int -> Registers -> Registers)
opcodes = Map.fromList
  [ ("addr", sem $ \reg a b -> reg a + reg b)
  , ("addi", sem $ \reg a b -> reg a + val b)

  , ("mulr", sem $ \reg a b -> reg a * reg b)
  , ("muli", sem $ \reg a b -> reg a * val b)

  , ("banr", sem $ \reg a b -> reg a .&. reg b)
  , ("bani", sem $ \reg a b -> reg a .&. val b)

  , ("borr", sem $ \reg a b -> reg a .|. reg b)
  , ("bori", sem $ \reg a b -> reg a .|. val b)

  , ("setr", sem $ \reg a _ -> reg a)
  , ("seti", sem $ \_   a _ -> val a)

  , ("gtir", sem $ \reg a b -> if val a > reg b then 1 else 0)
  , ("gtri", sem $ \reg a b -> if reg a > val b then 1 else 0)
  , ("gtrr", sem $ \reg a b -> if reg a > reg b then 1 else 0)

  , ("eqir", sem $ \reg a b -> if val a == reg b then 1 else 0)
  , ("eqri", sem $ \reg a b -> if reg a == val b then 1 else 0)
  , ("eqrr", sem $ \reg a b -> if reg a == reg b then 1 else 0)
  ]
  where
    sem f a b c regs = (IntMap.insert c $! f (regs IntMap.!) a b) regs
val v = v


-- | Get the input for the given day.
--
-- If a filename is provided in the command line that will be used as the
-- input file.
--
-- If the filename is @-@ the stdin will be used as the input file.
--
-- Otherwise the input text file corresponding to the day number will be used.
getRawInput :: Int {- ^ day number -} -> IO String
getRawInput i =
  do args <- getArgs
     case args of
       []    -> readFile (printf "inputs/input%02d.txt" i)
       "-":_ -> getContents
       fn:_  -> readFile fn

getInput :: Int -> IO [String]
getInput i = lines <$> getRawInput i

type Parser = Parsec Void String

getParsedInput :: Int -> Parser a -> IO a
getParsedInput i p =
  do input <- getRawInput i
     case parse p "input.txt" input of
       Left e -> fail (errorBundlePretty e)
       Right a -> return a

getParsedLines :: Int -> Parser a -> IO [a]
getParsedLines i p = getParsedInput i (many (p <* newline) <* eof)

-- | Count the number of elements in a foldable value that satisfy a predicate.
count :: Foldable t => (a -> Bool) -> t a -> Int
count p = foldl' (\acc x -> if p x then acc+1 else acc) 0


-- | Return true when the whole list is comprised of equal elements.
--
-- >>> same [1,1,1]
-- True
-- >>> same []
-- True
-- >>> same [1]
-- True
-- >>> same [1,1,2]
-- False
same :: Foldable t => Eq a => t a -> Bool
same xs = all (head (toList xs) ==) xs

-- | Returns a list of ways to select an element from a list without
-- replacement.
--
-- >>> pickOne []
-- []
-- >>> pickOne [1]
-- [(1,[])]
-- >>> pickOne [1,2,3]
-- [(1,[2,3]),(2,[1,3]),(3,[1,2])]
pickOne :: [a] -> [(a, [a])]
pickOne xs = [ (x, l++r) | (l,x:r) <- zip (inits xs) (tails xs) ]

-- | Parse a signed integral number
number :: Integral a => Parser a
number = signed (return ()) decimal

-- | Implementation of 'nub' that uses 'Ord' for efficiency.
ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | Set.member x seen = go seen xs
      | otherwise         = x : go (Set.insert x seen) xs


-- | Compute the minimum element of a list or return Nothing if it is empty.
--
-- >>> minimumMaybe []
-- Nothing
-- >>> minimumMaybe [2,1,3]
-- Just 1
minimumMaybe :: Ord a => [a] -> Maybe a
minimumMaybe xs
  | null xs   = Nothing
  | otherwise = Just $! minimum xs

-- | Compute the number of occurrences of the elements in a given list.
--
-- >>> cardinality "bababc"
-- fromList [('a',2),('b',3),('c',1)]
cardinality :: Ord a => [a] -> Map a Int
cardinality xs = Map.fromListWith (+) [ (x,1) | x <- xs ]