module Day1Part2 (
  part2
) where

import System.IO()
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Control.Monad()
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


findFirst :: [Int] -> IntSet -> Maybe Int
findFirst [] _ = Nothing
findFirst (x:xs) s
  | IntSet.member x s = Just x
  | otherwise = findFirst xs (IntSet.insert x s)

part2 = do
  contents <- readFile "numbers_2.txt"
  print $ findFirst (scanl1 (+) $ cycle (Prelude.map stringToInt . words $ contents)) IntSet.empty


