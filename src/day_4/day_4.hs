{-# LANGUAGE OverloadedStrings #-}

module Day4
    ( part1
    )
where


import qualified Data.Text                     as T
import           Data.Text.Read
import           Data.Sort
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Control.Monad.State
import           Data.List

data Time = Time {
    month :: Int,
    day:: Int,
    hours :: Int,
    minutes :: Int
} deriving (Show, Eq, Ord)

type GId = Int

data Event = Awake | Asleep | Start GId deriving (Show, Eq)

data TimeStamp = TimeStamp {
    time :: Time,
    event :: Event
} deriving (Show, Eq)

instance Ord TimeStamp where
    compare t1 t2 = compare (time t1) (time t2)

type Minute = Int
type MinutesAsleep = Int
type Count = Int
type TimeFeltAsleep = Time
type TimeWakeUp = Time
type MinuteCounter = Map Minute Count
type TimeAsleep = (GId, Map GId (TimeFeltAsleep, MinutesAsleep, MinuteCounter))

emptyTimeAsleep :: TimeAsleep
emptyTimeAsleep = (-1, M.empty)

readTime :: String -> Time
readTime ('[' : _ : _ : _ : _ : '-' : m1 : m2 : '-' : d1 : d2 : ' ' : h1 : h2 : ':' : min1 : min2 : _)
    = Time (read [m1, m2]) (read [d1, d2]) (read [h1, h2]) (read [min1, min2])

readEvent :: String -> Event
readEvent (x : xs) | x == 'G' = Start (read $ takeWhile (/= ' ') $ drop 6 xs)
                   | x == 'f' = Asleep
                   | x == 'w' = Awake

readTimeStamp :: String -> TimeStamp
readTimeStamp s = TimeStamp ((readTime . take 18) s) ((readEvent . drop 19) s)

{- Find guard most minutes asleep -}
{- Find minute he is most asleep -}

minutesBetween :: Time -> Time -> Int
minutesBetween (Time _ _ _ m1) (Time _ _ _ m2) = (m2 - m1)

increaseMinuteCounter :: MinuteCounter -> Minute -> MinuteCounter
increaseMinuteCounter counter min = case M.lookup min counter of
    Nothing -> M.insert min 1 counter
    Just a  -> M.insert min (a + 1) counter

updateMinutesCounter
    :: TimeFeltAsleep -> TimeWakeUp -> MinuteCounter -> MinuteCounter
updateMinutesCounter a@(Time _ _ _ mins1) w@(Time _ _ _ mins2) counter =
    let minutesList = [ x | x <- [mins1 .. mins2 - 1] ]
    in  foldl increaseMinuteCounter counter minutesList


handleAwake :: Time -> State TimeAsleep ()
handleAwake t = state $ \(id, m) ->
    let updated = case M.lookup id m of
            Nothing                             -> m
            Just (feltAsleep, asleep, minCount) -> M.insert
                id
                ( t
                , asleep + minutesBetween feltAsleep t
                , updateMinutesCounter feltAsleep t minCount
                )
                m
    in  ((), (id, updated))

handleAsleep :: Time -> State TimeAsleep ()
handleAsleep t = state $ \(id, m) ->
    let updated = case M.lookup id m of
            Nothing                    -> M.insert id (t, 0, M.empty) m
            Just (_, asleep, minCount) -> M.insert id (t, asleep, minCount) m
    in  ((), (id, updated))


handleStart :: GId -> State TimeAsleep ()
handleStart id = state $ \(_, m) -> ((), (id, m))

getSleepyGuard :: State TimeAsleep GId
getSleepyGuard = state $ \t@(_, m) ->
    ( fst $ maximumBy (\(_, (_, a, _)) (_, (_, b, _)) -> a `compare` b)
                      (M.assocs m)
    , t
    )

getSleepyMinute :: GId -> State TimeAsleep Minute
getSleepyMinute id = state $ \t@(_, m) -> case M.lookup id m of
    Nothing -> (-1, t)
    Just (_, _, counter) ->
        ( fst $ maximumBy (\(_, a) (_, b) -> a `compare` b) $ M.assocs counter
        , t
        )

handleInstruction :: TimeStamp -> State TimeAsleep ()
handleInstruction (TimeStamp t e) = case e of
    Asleep     -> handleAsleep t
    Awake      -> handleAwake t
    (Start id) -> handleStart id

getSpleeyGuardAndMinute :: State TimeAsleep ((GId, Minute), (GId, Minute))
getSpleeyGuardAndMinute = do
    id   <- getSleepyGuard
    min  <- getSleepyMinute id
    pair <- getMostSleepyMinute
    return ((id, min), pair)

getMaxMinute :: MinuteCounter -> (Minute, Count)
getMaxMinute m = maximumBy (\(_,c1) (_, c2) -> c1 `compare` c2) $ M.assocs m

getMostSleepyMinute :: State TimeAsleep (GId, Minute)
getMostSleepyMinute = state $ \t@(_, m) ->
    let (id, (_, _, counter)) =
            maximumBy
                    (\(_, (_, _, m1)) (_, (_, _, m2)) ->
                        snd (getMaxMinute m1) `compare`  snd (getMaxMinute m2)
                    )
                $ M.assocs m
    in  ((id, fst $ getMaxMinute counter), t)



part1 :: IO ()
part1 = do
    content <- readFile "challenge.txt"
    print $ evalState getSpleeyGuardAndMinute $ execState
        (mapM handleInstruction $ (sort . map readTimeStamp) (lines content))
        emptyTimeAsleep
