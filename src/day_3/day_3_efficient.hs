{-# LANGUAGE OverloadedStrings #-}

module Day3Efficient
    ( part1Efficient
    )
where

import qualified Data.Text                     as T
import           Data.List.Utils
import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as S

type Pos = (Int, Int)

data Claim = Claim { cId :: Int
                    , left :: Int
                    , top :: Int
                    , wide :: Int
                    , tall :: Int
} deriving (Show, Eq)

type Cell = (Pos, [Int])
type Fabric = Map Pos [Int]

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


claimInch :: Int -> Pos -> Fabric -> Fabric
claimInch cId pos f = M.insert pos row f
  where
    row = case M.lookup pos f of
        Nothing -> [cId]
        Just r  -> cId : r

generateIndicesFromClaim :: Claim -> [Pos]
generateIndicesFromClaim c =
    [ (x, y)
    | x <- [(left c) .. ((left c + wide c) - 1)]
    , y <- [(top c) .. ((top c + tall c) - 1)]
    ]

insertClaim :: Fabric -> Claim -> Fabric
insertClaim f c = foldl' (\fab pos -> claimInch (cId c) pos fab)
                         f
                         (generateIndicesFromClaim c)

countMultipleClaimSpots :: Fabric -> Int
countMultipleClaimSpots f = length $ M.filter (\x -> length x > 1) f

insertClaims :: [Claim] -> Fabric
insertClaims = foldl' insertClaim M.empty

toClaims :: String -> [Claim]
toClaims = map toClaim . lines

claimSize :: String -> Map Int Int
claimSize s = foldl' (\cs c -> (M.insert (cId c) ((wide c) * (tall c)) cs))
                     (M.empty)
                     (toClaims s)

getNumberOfInches :: Int -> Fabric -> Int
getNumberOfInches id f = M.size $ M.filter (\e -> (head e) == id) f

findValidClaim :: Map Int Int -> Fabric -> Maybe Int
findValidClaim m f = find
    (\cId -> getNumberOfInches cId f == M.findWithDefault (-1) cId m)
    (S.elems $ S.fromList $ map head $ M.elems f)

part1Efficient :: IO ()
part1Efficient = do
    content <- readFile "challenge.txt"
    print $ (countMultipleClaimSpots . insertClaims . toClaims) content

part2Efficient :: IO ()
part2Efficient = do
    content <- readFile "challenge.txt"
    print
        $ ( findValidClaim (claimSize content)
          . M.filter (\x -> length x == 1)
          . insertClaims
          . toClaims
          )
              content

