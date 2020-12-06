module Main where

import Common

main :: IO ()
main = do
  numbers <- input
  let (x, y) =  correctPair $ numberPairs numbers
  print $ x * y

input :: IO [Integer]
input = do
  contents <- readFile "input/day1.1"
  return $ map read $ lines contents

numberPairs :: [Integer] -> [(Integer, Integer)]
numberPairs numbers = [ (x, y) | x <- numbers, y <- numbers ]

correctPair :: [(Integer, Integer)] -> (Integer, Integer)
correctPair numbers = head $ filter sumsTo2020 numbers

sumsTo2020 :: (Integer, Integer) -> Bool
sumsTo2020 (x, y) = x + y == 2020
