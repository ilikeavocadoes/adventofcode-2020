module Main where

import Protolude

inputRaw :: IO Text
inputRaw = readFile "input/day5.1"

input :: IO [Text]
input = do
  contents <- inputRaw
  return $ lines contents

data Seat = Seat Partition Partition Partition Partition Partition Partition Partition Partition Partition Partition
