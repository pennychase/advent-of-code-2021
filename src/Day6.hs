{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq (..), (<|), (|>) )
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read as T
import qualified Data.MultiSet as MS
import Data.MultiSet (MultiSet)

-- Part 1
-- Of course, Part 1 can be solved using the Part 2 code

updateSim :: Seq Int -> Seq Int -> Seq Int 
updateSim cur new =
    case cur of
        Seq.Empty -> new
        (x :<| xs) -> if x == 0 
                        then 6 <| (updateSim xs (new |> 8))
                        else (x-1) <| (updateSim xs new)

part1 :: Int -> [Int] -> Int 
part1 n lst = Seq.length $ 
                iterate (\x -> updateSim x Seq.empty) (Seq.fromList lst) !! n

-- Part 2

-- To handle the exponential growth, we will use a Map to track the number of fish with
-- each timer value between 0 and 8. For each step, we reduce the time value and keep the
-- number of fish, except for 0 (the timer value becomes 6 and new fish with timer value of 8 are added).

-- Create a map with all 0 values for our folding
makeNewMap :: Map Int Int
makeNewMap = M.fromList $ zip [0..8] (cycle [0])

-- Create the Map from our input
initializeMap :: [Int] -> Map Int Int
initializeMap ns =
    foldr insertFunc makeNewMap ns
    where
        insertFunc k m = M.adjust (+1) k m

updateOne :: Int -> Map Int Int -> Map Int Int -> Map Int Int
updateOne key oldMap newMap =
    if key == 0 
        then M.insert 6 update6 (M.insert 8 update8 newMap)
        else M.insert (key - 1) (getVal key) newMap
    where
        getVal k = fromJust (M.lookup k oldMap)  -- will always be Just since called with oldMap's keys
        update6 = getVal 0 + getVal 7  -- the current 0's and 7's necome 6's in the next step
        update8 = getVal 0  -- each 0 adds an 8 to the next step

updateMap :: Map Int Int -> Map Int Int 
updateMap curMap =
    foldr (\k -> updateOne k curMap) makeNewMap keys
    where
        keys = map fst $ filter ((> 0) . snd) (M.assocs curMap)

howMany :: Map Int Int -> Int 
howMany m = M.foldr (+) 0 m

part2 :: Int -> [Int] -> Int 
part2 n lst = howMany $ iterate updateMap (initializeMap lst) !! n

-- Main 

main :: IO ()
main = do
    contents <- TIO.readFile "./data/day6-input.txt"
    case mapM T.decimal $ T.splitOn "," contents of
        Left _ -> putStrLn "Invalid input"
        Right input -> do 
            putStrLn $ "Part 1: " <> show (part1 80 (map fst input))
            putStrLn $ "Part 2: " <> show (part2 256 (map fst input))
