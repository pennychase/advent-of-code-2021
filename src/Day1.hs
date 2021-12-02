{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Data.List ( tails, transpose )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as T
import Fmt

data Part = Part1 | Part2
    deriving (Show, Eq)

-- For part 1 and 2
-- For a list of numbers determine how many are larger than their predecessor
-- by subtracting orginal list from the tail of the list, and counting how many of
-- the difference are positive
largerThanPrevious :: [Int] -> Int 
largerThanPrevious ns =
    length $ filter (>0) $ zipWith (-) (tail ns) ns

-- For part 2
-- Create a list by summing a sliding window of three values, and then repeat part1 on
-- that list. Use (take 3 $ tails) to create the sliding window. The list is really a list of rows and
-- we want to sum the columns, so use transpose and filter out columns with fewer than 3 elements
-- (suggested in stack overflow by random-dev:
-- https://stackoverflow.com/questions/33122865/sum3-with-zipwith3-in-haskell)
largerThanWindow :: [Int] -> Int 
largerThanWindow ns = 
    largerThanPrevious $ map sum $ filter (\y -> length y == 3) $ transpose $ take 3 $ tails ns

-- Takes Text input, converts to Ints, and processes part 1 or part 2
process :: Part -> Text -> Either String (Int, Int)
process part content =
    let 
        nums = mapM T.decimal $ T.lines content
    in 
        case nums of
            Right pairs -> case part of
                Part1 -> pure $ (1, largerThanPrevious (map fst pairs))
                Part2 -> pure $ (2, largerThanWindow (map fst pairs))
            Left _ -> Left "Invalid input"

-- Outputs the processing results 
output :: Either String (Int, Int) -> IO ()
output result = do
    case result of
        Left err -> TIO.putStrLn $ T.pack err
        Right (n, result') -> TIO.putStr $ fmt $ "Part "+|n|+": "+|result'|+"\n"

main :: IO ()
main = do
    contents <- TIO.readFile "./data/day1-part1.txt"
    output $ process Part1 contents
    output $ process Part2 contents

 

-- Test Data
testData :: [Int]
testData = [199,200,208,210,200,207,240,269,260,263]
