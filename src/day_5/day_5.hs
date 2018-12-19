module Day5
    ( solve
    )
where

import           Data.Char

rangeBetweenUpperAndLower = 32

isReactivePair :: Char -> Char -> Bool
isReactivePair c1 c2 = abs (ord c1 - ord c2) == rangeBetweenUpperAndLower

reduceOnce :: String -> String
reduceOnce (x1 : x2 : xs) | isReactivePair x1 x2 = reduceOnce xs
                          | otherwise            = x1 : reduceOnce (x2 : xs)
reduceOnce xs = xs

reduceAll :: String -> String -> String
reduceAll reduced original | reduced == original = reduced
                           | otherwise = reduceAll (reduceOnce reduced) reduced


reduce :: String -> String
reduce s = reduceAll (reduceOnce s) s

solve :: String -> IO ()
solve fileName = do
    content <- readFile fileName
    print $ (length . reduce) content
