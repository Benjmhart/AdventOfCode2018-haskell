import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.List
import Data.Char
import Data.Function


main :: IO()
main = do
  input <- readFile $ "./inputs/day4-1sorted.txt"
  let strList = lines $ input
  putStr $ show . solve $ strList
  return ()

sortIOList :: IO ()
sortIOList = do
  input <- readFile $ "./inputs/day4-1.txt"
  let output = unlines . sort . lines $ input
  writeFile "./inputs/day4-1sorted.txt" output
  return ()

-- takes raw input, accumulator of dateString to ID returns (list of Id's, map of ID to relevant )
type DateTimeWithId = ((Integer, Integer, Integer), Integer)




-- get the DList of guardId's & minutes asleep
-- sort all lists - get dupes next to each other
-- group all lists - make a list of lists of identical elements
-- sort all lists by sublist length - get the most common element to head
-- Head all lists - discard useless sublists , possibly cause exception
-- catMaybes -- only if exception is hit, use safehead for above step, msy need a tuple catMayBe implementation
-- sortSndLength - get the tuple with the most common element to the head
-- head - discard the losers
-- multiply the two values for solution

solve :: [String] -> Integer
solve xs = id * time
  where 
    (id, time) = headSnd . head . sortSndLength . catMaybeSnd . fmap prepTuple . M.toList . collectInfo xs M.empty $ ((59,0,0), 0)
    
prepTuple :: (Integer, (a, [Integer])) -> (Integer, Maybe[Integer])
prepTuple = safeHeadSnd . sortLengthSnd . groupSnd . sortSnd . transformTuple
    
transformTuple :: (t2, (t, t1)) -> (t2, t1)
transformTuple (y, (_, ms)) = (y, ms)
sortSnd (a, bs) = (a, sort bs)
groupSnd (a,bs) = (a, group bs)

catMaybeSnd :: [(a, Maybe b)] -> [(a,b)]
catMaybeSnd []               = []
catMaybeSnd ((_,Nothing):xs) = catMaybeSnd xs
catMaybeSnd ((a,Just b):xs)  = (a,b):catMaybeSnd xs

headSnd (a, b) = (a, head b)

safeHeadSnd :: (a,[b])->(a, Maybe b)
safeHeadSnd (a,[]) = (a, Nothing)
safeHeadSnd (a,bs) = (a, (Just $head bs))

sortLengthSnd (a,bs) = (a, sortBy sortLength bs)

sortSndLength :: [(a,[a])] -> [(a,[a])]
sortSndLength = sortBy (sortLength `on` snd)

sortLength :: [a] -> [a] -> Ordering
sortLength a b
  | length a <  length b = GT
  | length a >  length b = LT
  | length a == length b = EQ  

-- takes list of entries, records a map with the guard's last action and total sleep time, increments time until done, returns a map
collectInfo :: [String] -> M.Map Integer (String, [Integer]) -> DateTimeWithId ->  M.Map Integer (String, [Integer]) 
collectInfo []       mapInfoById _       = mapInfoById 
collectInfo (xs:xss) mapInfoById date@((_,_,minute),id'')
  | (((not isAsleep) && firstTime) && id /= 0) && advance = collectInfo xss firstTimeMap (recordDate, id)
  | ((not isAsleep) && firstTime) && id /= 0  = collectInfo (xs:xss) firstTimeMap nextDate
  | ((not isAsleep) && firstTime) && advance = collectInfo xss mapInfoById (recordDate, id)
  | not isAsleep && advance     = collectInfo xss nextMapNoSleep (recordDate, id)
  | not isAsleep                = collectInfo(xs:xss) nextMapNoSleep nextDate
  | (isAsleep && advance)       = collectInfo xss nextMapWithSleep (recordDate, id)
  | isAsleep                    = collectInfo (xs:xss) nextMapWithSleep nextDate
  where firstTime               = isNothing $ lookupResult
        rDate@(recordDate, id')  = fromArbitraryString xs
        id                      = if id' == 0 then id'' else id'
        lookupResult            = M.lookup id mapInfoById
        firstTimeMap            = M.insert id (nextAction, []) mapInfoById
        nextMapNoSleep          = M.insert id (nextAction, sleepTimes) mapInfoById
        nextMapWithSleep        = M.insert id (nextAction, (minute:sleepTimes)) mapInfoById
        nextAction              = if advance then currentAction else lastAction
        nextDate                = changeId id $ incMinute date
        currentAction           = breakAt xs ']'
        isAsleep                = ((safeHead sleepTimes /= (Just minute)) || currentAction ==" falls asleep") && (nextAction == " falls asleep")
        advance                 = recordDate == (fst date) || "begins" `isInfixOf` currentAction
        lastAction              = if isJust lookupResult then fst . fromJust $ lookupResult else ""
        sleepTimes              = if isJust lookupResult then snd . fromJust $ lookupResult else []


safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

breakAt :: String -> Char -> String
breakAt []     _ = []
breakAt (x:xs) c 
  | x == c  = xs
  | otherwise = breakAt xs c

changeId :: Integer -> DateTimeWithId -> DateTimeWithId
changeId id ((d,h,m),_) = ((d,h,m),id)

incMinute :: DateTimeWithId -> DateTimeWithId
incMinute ((d,h,m),id) = ((days, hours, minutes), id)
  where nextHour = m == 59
        nextDay  = h == 23
        minutes  = if nextHour then 0 else m+1
        hours    = if nextHour then (if nextDay then 0 else h+1) else h
        days     = if nextHour && nextDay then d+1 else d


fromArbitraryString :: String -> DateTimeWithId
fromArbitraryString xs = (((getDays mo d), h, m), id)
    where dateId@(mo:d:h:m:_) =  map toInteger' . tail . words . map (toSpace) $ xs
          id = if (length dateId) > 4 then dateId !! 4 else 0
          getDays month days
            | month == 3  = 60  + day-- 31
            | month == 4  = 91  + day-- 30
            | month == 5  = 121 + day-- 31
            | month == 6  = 152 + day-- 30
            | month == 7  = 182 + day-- 31
            | month == 8  = 213 + day-- 31
            | month == 9  = 244 + day-- 30
            | month == 10 = 274 + day-- 31
            | month == 11 = 305 + day-- 30
            | month == 12 = 335 + day-- 31
            where day = days - 1

toInteger' x = (read x) :: Integer

toSpace :: Char -> Char
toSpace c
  | isDigit c = c
  | otherwise = ' '

