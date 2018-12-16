{-# LANGUAGE OverloadedStrings #-}

module Day3
    ( part1
    )
where


import qualified Data.Text                     as T
import           Data.List.Utils
import           Data.List

type Pos = (Int, Int)

data Claim = Claim { cId :: Int
                    , left :: Int
                    , top :: Int
                    , wide :: Int
                    , tall :: Int
} deriving (Show, Eq)

data Fabric = Fabric { field :: [[[Int]]]} deriving (Show, Eq)


toClaim :: String -> Claim
toClaim line =
    let [cId, left, top, wide, tall] =
            words
                $ T.unpack
                $ T.replace "#" ""
                $ T.replace "@" ""
                $ T.replace "," " "
                $ T.replace ":" ""
                $ T.replace "x" " "
                $ T.pack line
    in  Claim { cId  = read cId
              , left = read left
              , top  = read top
              , wide = read wide
              , tall = read tall
              }

createFabric :: Int -> Fabric
createFabric size = Fabric { field = replicate size (replicate size []) }

modifyRow :: Int -> Int -> [[Int]] -> [[Int]]
modifyRow cId i r = fst ++ [concat (repl ++ [[cId]])] ++ (drop 1 snd)
  where
    (fst, snd) = splitAt i r
    repl       = take 1 snd



claimInch :: Int -> Pos -> [[[Int]]] -> [[[Int]]]
claimInch cId (left, top) l =
    fst ++ [(modifyRow cId left repl)] ++ (drop 1 snd)
  where
    (fst, snd) = splitAt top l
    repl       = (take 1 snd) !! 0

generateIndicesFromClaim :: Claim -> [Pos]
generateIndicesFromClaim c =
    [ (x, y)
    | x <- [(left c) .. ((left c + wide c) - 1)]
    , y <- [(top c) .. ((top c + tall c) - 1)]
    ]

insertClaim :: Fabric -> Claim -> Fabric
insertClaim f c = inserted
  where
    inserted = foldl'
        (\fab pos -> Fabric { field = claimInch (cId c) pos (field fab) })
        f
        (generateIndicesFromClaim c)

countMultipleClaimSpots :: Fabric -> Int
countMultipleClaimSpots f =
    length $ filter (\x -> length x > 1) (concat (field f))

part1 :: IO ()
part1 = do
    content <- readFile "challenge.txt"
    print
        $ countMultipleClaimSpots
        $ foldl' insertClaim (createFabric 1000)
        $ map toClaim (lines content)

