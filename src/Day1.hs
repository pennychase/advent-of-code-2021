{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Data.List ( transpose )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as T
import Fmt

-- For part 1 and 2
largerThanPrevious :: [Int] -> Int 
largerThanPrevious ns =
    length $ filter (>0) $ zipWith (-) (tail ns) ns

-- For part 2
largerThanWindow :: [Int] -> Int 
largerThanWindow ns = 
    largerThanPrevious $ map sum $ filter (\y -> length y == 3) $ transpose [ns, tail ns, tail (tail ns)]

process :: Text -> ([Int] -> Int) -> Either String Int
process content func = 
    let 
        ls = T.lines content
    in 
        case mapM T.decimal ls of
            Right pairs -> pure $ func (map fst pairs)
            Left _ -> Left "Invalid input"


part :: Int -> ([Int] -> Int) -> FilePath -> IO ()
part n func filename = do
    content <- TIO.readFile "./data/day1-part1.txt"
    case process content func of
        Left err -> TIO.putStrLn $ T.pack err
        Right result -> TIO.putStr $ fmt $ "Part "+|n|+" result: "+|result|+"\n"

main :: IO ()
main = do
    part 1 largerThanPrevious "./data/day1-part1.txt"
    part 2 largerThanWindow "./data/day1-part1.txt"
 

-- Test
testData :: [Int]
testData = [199,200,208,210,200,207,240,269,260,263]
