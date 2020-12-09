module Main where

import Protolude

import Data.Text.Read (decimal)

main :: IO ()
main = part1 >> part2

part1 :: IO ()
part1 = numbers >>= print . searchNonSum

part2 :: IO ()
part2 = do
  ns <- numbers
  (print . searchContiguous ns . searchNonSum) ns

input :: IO [Text]
input = lines <$> readFile "input/day9.input"

numbers :: IO [Integer]
numbers = input >>= pure . map (fst . fromRight undefined . decimal)

type Window = [Integer]

isSumOfNumbers :: [Integer] -> Integer -> Bool
isSumOfNumbers xs n = foldl (||) False $ map (== n) $ pairSums xs

pairSums :: [Integer] -> [Integer]
pairSums (x:xs) = map (+ x) xs ++ pairSums xs
pairSums [] = []

searchNonSum :: [Integer] -> Integer
searchNonSum numbers =
  go (take 25 numbers) (drop 25 numbers)
  where
    go window (current:rest) =
      if isSumOfNumbers window current
        then go (take 25 (current:window)) rest
        else current

searchContiguous :: [Integer] -> Integer -> Integer
searchContiguous numbers n =
  aoeu 0 [] 0 numbers numbers
  where
    aoeu acc window windowSize (l:last) (f:first)
      | acc == n = maximum window + minimum window
      | acc > n = aoeu (acc - l) (take (windowSize - 1) window) (windowSize - 1) last (f:first)
      | otherwise = aoeu (acc + f) (f:window) (windowSize + 1) (l:last) first
