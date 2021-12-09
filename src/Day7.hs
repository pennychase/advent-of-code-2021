module Day7 where

import Data.List
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Parser

cost1 :: (Int, Int) -> Int
cost1 (steps, wt) = steps * wt

cost2 :: (Int, Int) -> Int
cost2 (steps, wt) = sum [1..steps] * wt

calculateCost :: ((Int, Int) -> Int) -> MultiSet Int -> Int -> (Int, Int)
calculateCost cf initPos pos = 
    (pos, cost)
    where
        distances = MS.map (abs . (pos -)) initPos
        cost = sum $ map cf (MS.toOccurList distances)

findSmallest :: ((Int, Int) -> Int)  -> MultiSet Int -> (Int, Int)
findSmallest cf initPos =
        minimumBy (\x y -> compare (snd x) (snd y)) $ map (calculateCost cf initPos) positions
    where
        positions = [MS.findMin initPos .. MS.findMax initPos]

part1 :: [Int] -> (Int, Int)
part1 positions = findSmallest cost1 $ MS.fromList positions

part2 :: [Int] -> (Int, Int)
part2 positions = findSmallest cost2 $ MS.fromList positions

main :: IO ()
main = do
    positions <- readInput "data/day7-input.txt" csvParser
    putStrLn $ "Part 1: " <> show (part1 positions)
    putStrLn $ "Part 2: " <> show (part2 positions)
