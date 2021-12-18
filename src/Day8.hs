{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Data.Bits
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Observation = ([Text], [Text])

data Segment = Seg6 | Seg5 | Seg4 | Seg3 | Seg2 | Seg1 | Seg0
    deriving (Show, Eq, Ord, Enum)

-- Part 2
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

charToSegMap :: String -> Map Char Segment
charToSegMap s = M.fromList $ zip s [Seg6, Seg5 .. ]

strToSegs :: Text -> Map Char Segment -> [Segment]
strToSegs s m = mapMaybe (\x -> M.lookup x m) (T.unpack s)

strToDigit :: Text -> Map Char Segment -> Maybe Int
strToDigit s m = segsToDigit (strToSegs s m)

testPermutation :: String -> [Text] -> Maybe (Map Char Segment)
testPermutation perm xs = 
    case mapM (flip strToDigit m) xs of
        Nothing -> Nothing 
        _       -> pure m
    where
        m = charToSegMap perm

testPermutations :: Observation -> Maybe [Int]
testPermutations obs =
    go perms
    where
        perms = permutations "abcdefg"
        go [] = Nothing
        go (p:ps) =
            case testPermutation p (fst obs) of
                Nothing -> go ps
                Just m  -> mapM (flip strToDigit m) (snd obs)

digitsToInt :: [Int] -> Int 
digitsToInt ns = foldl' (\ds d -> 10*ds + d) 0 ns

part2 :: [Observation] -> Int
part2 obs = 
    sum $ map digitsToInt (mapMaybe testPermutations obs)

-- Part 1

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
    putStrLn $ "Part 2: " <> show (part2 observations)    

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