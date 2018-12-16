module Day2 (
  part1,
  part2
) where

import Data.List
import Data.List.Utils
import qualified Data.Set as Set

countOccurences :: [(Char,Integer)] -> Char -> [(Char,Integer)]
countOccurences l c = increaseCounter c (lookup c l) l

filterN :: [[(Char, Integer)]] -> Integer -> [[(Char, Integer)]]
filterN l n = filter (\x -> (length x) > 0) $ map (\x -> filter (\t -> snd t == n) x) l


countN :: [[(Char, Integer)]] -> Integer -> Int
countN l n = length $ filterN l n

increaseCounter :: Char -> Maybe Integer -> [(Char, Integer)] -> [(Char, Integer)]
increaseCounter c (Just a) list = addToAL list c (a + 1)
increaseCounter c Nothing list = addToAL list c 1

test = [[('d',2),('e',1),('c',1),('b',1),('a',1)],[('c',1),('b',3),('a',2)],[('e',1),('d',1),('c',1),('b',2),('a',1)],[('d',1),('c',3),('b',1),('a',1)],[('d',2),('c',1),('b',1),('a',2)],[('e',2),('d',1),('c',1),('b',1),('a',1)],[('b',3),('a',3)]]

checksum :: [String] -> Int
checksum ids = (countN occurences 2) * (countN occurences 3)
  where occurences = map (foldl countOccurences []) ids


correctBoxes :: (String, String) -> Bool
correctBoxes (a, b) = length a == length b && length (filter (uncurry (/=)) zipped) == 1
  where zipped = (zip a b)

findIDs :: [String] -> Maybe (String, String)
findIDs ids = find correctBoxes [(x,y) | x <- ids, y <- ids]

removeUncommonLetters :: (String, String) -> [Char]
removeUncommonLetters (a, b) = map fst (filter (uncurry (==)) zipped)
  where zipped = zip a b

commonLetters :: [String] -> Maybe [Char]
commonLetters l = fmap removeUncommonLetters (findIDs l)

readInput :: IO String
readInput = readFile "challenge.txt"

checksumMain :: IO ()
checksumMain = do
  contents <- readInput
  print $ checksum (words contents)

part1 :: IO ()
part1 = do
  contents <- readInput
  print $ findIDs (words contents)

part2 :: IO ()
part2 = do
  contents <- readInput
  print $ commonLetters (words contents)

