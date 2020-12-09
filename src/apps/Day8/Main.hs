{-# LANGUAGE OverloadedStrings #-}
module Main where
  
import Protolude

import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)
import Numeric.Natural (Natural)
import qualified Data.Set as Set
import Data.Set (Set)
import Common

main :: IO ()
main = do
  input <- inputRaw
  let instructions =
        case parseInput input of
          Nothing -> undefined
          Just i -> i
      rows = getRows instructions
      rowZipper = case fromList rows of
        Nothing -> undefined
        Just z -> z
  case runStateT (stepRow rowZipper) (MachineState 0 Set.empty) of
    Nothing -> undefined
    Just (i, _) -> print i

inputRaw :: IO Text
inputRaw = readFile "input/day8.input"

stepRow :: ListZipper Row -> StateT MachineState Maybe Integer
stepRow rows = do
  MachineState acc visited <- get
  if Set.member (focus rows) visited
    then return acc
    else executeInstruction rows >>= stepRow

data MachineState = MachineState Integer (Set Row)

executeInstruction :: ListZipper Row -> StateT MachineState Maybe (ListZipper Row)
executeInstruction zipper = do
  case focus zipper of
    Row i (Acc x) -> do
      MachineState current visited <- get
      put $ MachineState (current + x) (Set.insert (Row i (Acc x)) visited)
      lift $ stepRight zipper
    Row i (Jmp x) -> do
      MachineState current visited <- get
      put $ MachineState current (Set.insert (Row i (Jmp x)) visited)
      if x < 0
        then lift $ doNTimes (fromIntegral (-x)) stepLeft zipper
        else lift $ doNTimes (fromIntegral x) stepRight zipper
    Row i Nop -> do
      MachineState current visited <- get
      put $ MachineState current (Set.insert (Row i Nop) visited)
      lift $ stepRight zipper

doNTimes :: Monad m => Natural -> (a -> m a) -> a -> m a
doNTimes 0 f x = pure x
doNTimes n f x = f x >>= doNTimes (n-1) f

data Instruction = Acc Integer | Jmp Integer | Nop deriving (Eq, Ord, Show)

data Row = Row Integer Instruction deriving (Eq, Ord, Show)

getRows :: [Instruction] -> [Row]
getRows = zipWith Row [0..]

parseInput :: Text -> Maybe [Instruction]
parseInput input = case P.parseOnly parseInstructions input of
  Right is -> Just is
  Left _ -> Nothing

parseInstructions :: Parser [Instruction]
parseInstructions = P.many' parseInstruction

parseInstruction :: Parser Instruction
parseInstruction = do
  instruction <- parseAcc <|> parseJmp <|> parseNop
  P.endOfLine
  return instruction

parseAcc :: Parser Instruction
parseAcc = do
  _ <- P.string "acc"
  P.skipSpace
  arg <- P.signed P.decimal
  return $ Acc arg

parseJmp :: Parser Instruction
parseJmp = do
  _ <- P.string "jmp"
  P.skipSpace
  arg <- P.signed P.decimal
  return $ Jmp arg

parseNop :: Parser Instruction
parseNop = do
  _ <- P.string "nop"
  P.skipSpace
  _ <- P.signed P.decimal :: Parser Integer
  return $ Nop
