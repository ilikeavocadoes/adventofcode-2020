{-# LANGUAGE NamedFieldPuns #-}

import           Protolude

import           Data.Maybe         (fromJust)
import           Data.Text          (pack, splitOn)
import           Data.Text.Read     (decimal)
import           Text.Pretty.Simple (pPrint)

main :: IO ()
main = input >>= putText . problem1 >> input >>= putText . problem2

-- main = input >>= putText . problem1 >> pure input >>= putText . problem2
input :: IO Text
input = readFile "input/day13.input"

problem1 :: Text -> Text
problem1 timetable =
  let (tStartTime, tTimes) = splitInput timetable
      startTime = fst . fromRight undefined $ decimal tStartTime
      buses = parseBuses tTimes
      waitTimes = map (\n -> n - startTime `mod` n) buses
      busWaitTimes = zipWith BusWaitTime buses waitTimes
      shortest = minimum busWaitTimes
   in show $ busNumber shortest * waitTime shortest

data BusWaitTime = BusWaitTime
  { busNumber :: Integer
  , waitTime  :: Integer
  } deriving (Eq, Show)

instance Ord BusWaitTime where
  b1 <= b2 = waitTime b1 <= waitTime b2

splitInput :: Text -> (Text, Text)
splitInput inpt =
  let (startTime:busTimes:[]) = lines inpt
   in (startTime, busTimes)

parseBuses :: Text -> [Integer]
parseBuses timetable =
  foldr
    (\x xs ->
       case decimal x of
         Left _       -> xs
         Right (n, _) -> n : xs)
    [] $
  splitOn "," timetable

data BusTargetTime = BusTargetTime
  { targetBusNumber :: Integer
  , targetTime      :: Integer
  } deriving (Show, Eq)

instance Ord BusTargetTime where
  b1 <= b2 = targetBusNumber b2 <= targetBusNumber b2

parseTargetTimes :: Text -> [BusTargetTime]
parseTargetTimes timetable =
  foldr
    (\(x, t) xs ->
       case decimal x of
         Left _       -> xs
         Right (n, _) -> (BusTargetTime n t) : xs)
    [] $
  zip (splitOn "," timetable) [0,1 ..]

problem2 :: Text -> Text
problem2 timetable =
  let (_, tTimes) = splitInput timetable
      targetTimes = parseTargetTimes tTimes
   in show $ solveCrt targetTimes

isCorrectSolution :: [BusTargetTime] -> Integer -> Bool
isCorrectSolution targetTimes n =
  all
    (\BusTargetTime {targetBusNumber, targetTime} ->
       (n + targetTime) `mod` targetBusNumber == 0)
    targetTimes

sieve :: [BusTargetTime] -> Integer
sieve (t:targetTimes) = go targetTimes (targetBusNumber t) (targetTime t)
  where
    go [] step x2 = x2
    go (b2:bs) step x2 =
      let findCongruent (x:xs) modulo remainder
            | x `mod` modulo == remainder = x
            | otherwise = findCongruent xs modulo remainder
          n3 = targetBusNumber b2
          a3 = targetBusNumber b2 - targetTime b2
          candidates = map (+ x2) [0,step ..]
          xn = findCongruent candidates n3 a3
       in go bs (step * n3) xn

candidates :: Integer -> Integer -> [Integer]
candidates congruence step = map (+ congruence) [0,step ..]

solveCrt :: [BusTargetTime] -> Integer
solveCrt times =
  let n = product $ map targetBusNumber times
   in (`mod` n) . sum $
      map
        (\BusTargetTime {targetBusNumber, targetTime} ->
           (targetBusNumber - targetTime) *
           euclidianInverse (n `div` targetBusNumber) targetBusNumber *
           (n `div` targetBusNumber))
        times

euclidianInverse :: Integer -> Integer -> Integer
euclidianInverse a n =
  let t = 0
      newt = 1
      r = n
      newr = a
   in go r newr t newt
  where
    go r newr t newt
      | newr == 0 = t
      | otherwise =
        let quotient = r `div` newr
         in go newr (r - quotient * newr) newt (t - quotient * newt)

ex1 :: Text
ex1 = "53\n17,x,13,19"
