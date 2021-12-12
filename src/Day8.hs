{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Data.Bits
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Fixed (E0)

data Segment = Seg6 | Seg5 | Seg4 | Seg3 | Seg2 | Seg1 | Seg0
    deriving (Show, Eq, Ord, Enum)

segToBit :: Segment -> Int
segToBit = fromEnum

segsToDigit :: [Segment] -> Maybe Int 
segsToDigit segs = 
    case foldl' setBit (0::Int) (map segToBit segs) of
        0x7E -> Just 0
        0x30 -> Just 1
        0x6D -> Just 2
        0x79 -> Just 3
        0x33 -> Just 4
        0x5B -> Just 5
        0x5F -> Just 6
        0x70 -> Just 7
        0x7f -> Just 8
        0x7B -> Just 9
        _    -> Nothing






type Observation = ([Text], [Text])

getOutputSegLen :: Observation -> [Int]
getOutputSegLen  obs = map T.length $ snd obs

parseInput :: Text -> [Observation]
parseInput input =
    map parseLine (T.lines input)
    where
        parseLine line = 
            let
                line' = map T.words $ T.splitOn " | " line
            in (line' !! 0, line' !! 1)

part1 :: [Observation] -> Int
part1 obs = length $ filter (`elem` uniqueLengths) $ concatMap getOutputSegLen obs
    where uniqueLengths = [2,3,4,7]

main :: IO ()
main = do
    contents <- T.readFile "data/day8-input.txt"
    let observations = parseInput contents
    putStrLn $ "Part 1: " <> show (part1 observations)

-- Data
sample :: Text
sample = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n\
\edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n\
\fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n\
\fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n\
\aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n\
\fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n\
\dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n\
\bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n\
\egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n\
\gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"