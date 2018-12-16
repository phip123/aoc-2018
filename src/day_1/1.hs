module Day1Part1 (
  part1
) where

import System.IO
import Control.Monad

data Op  = Pos | Neg deriving (Show)

charToOp :: Char -> Op
charToOp c
  | c == '-' = Neg
  | otherwise = Pos

splitInput :: String -> (Op, Int)
splitInput x = ((charToOp $ head x), (read (tail x) :: Int))

opToInt :: (Op, Int) -> Int
opToInt (Pos, a) = a
opToInt (Neg, a) = -a

stringToInt :: String -> Int
stringToInt x = opToInt $ splitInput x

test = charToOp

part1 = do
  contents <- readFile "numbers.txt"
  print $ sum $ map stringToInt . words $ contents
