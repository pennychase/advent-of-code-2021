module Day3 where

import Data.Char ( digitToInt )
import Data.List ( transpose, foldl' )

type LCB = Int 
type MCB = Int 
type Epsilon = Int 
type Gamma = Int

data Criteria = Oxygen | CO2
    deriving (Show, Eq)

--
-- Utilities
--
bitsToInt :: [Int] -> Int 
bitsToInt = foldl' (\acc x -> acc * 2 + x) 0

binaryStrToDecimal :: String -> Int 
binaryStrToDecimal = foldl' (\acc x -> acc * 2 + digitToInt x) 0

digitToChar :: Int -> Char
digitToChar n = head . show $ n

--
-- Common
--

-- Find the least and most common bits in a single bit position
-- Count the zeros and ones in the bit position (represented as a String)
countBits :: String -> (Int, Int)
countBits bits = (zeros, ones)
    where 
        zeros = length $ filter (=='0') bits
        ones = length $ filter (=='1') bits

-- From the counts, determine which bit value is least common and most common
leastAndMostCommonBit :: (Int, Int) -> (LCB, MCB)
leastAndMostCommonBit (z, o) =
    if z > o then (1, 0) else (0, 1)

--
-- Part 1
--

-- Find least and most common bits in all bit positions. 
leastAndMostCommonBits :: [String] -> [(LCB, MCB)]
leastAndMostCommonBits positions = map (leastAndMostCommonBit . countBits) positions

part1 :: [String] -> Int 
part1 bits = epsilon * gamma
    where
        lcmcBits = leastAndMostCommonBits $ transpose bits
        epsilon = bitsToInt $ map fst lcmcBits
        gamma = bitsToInt $ map snd lcmcBits
        
--
-- Part 2
--

digitTest :: Criteria -> String -> Char
digitTest criteria bits =
    let
        counts@(zeros, ones) = countBits bits
        (lcb, mcb) = leastAndMostCommonBit counts
    in case criteria of
        Oxygen -> if zeros == ones then '1' else digitToChar mcb
        CO2 -> if zeros == ones then '0' else digitToChar lcb
           

findRating :: Criteria -> [String] -> Int 
findRating criteria rows = go 0 rows
    where
        go pos rs =
            let 
                digit = digitTest criteria $ map (!! pos) rs
                newRows = filter (\x -> digit == x !! pos) rs 
            in case newRows of
                [] -> 0
                [row] -> binaryStrToDecimal row
                _ -> go (pos + 1) newRows 

oxygenRating :: [String] -> Int 
oxygenRating bits = findRating Oxygen bits

co2Rating :: [String] -> Int 
co2Rating bits = findRating CO2 bits

part2 :: [String] -> Int 
part2 bits = oxygenRating bits * co2Rating bits

-- Main

main :: IO ()
main = do
    content <- readFile "./data/day3-input.txt"
    let rows = lines content
    putStrLn $ "Part 1: " <> show (part1 rows)
    putStrLn $ "Part 2: " <> show (part2 rows)

-- Test Data
testData :: [String]
testData = [  "00100"
            , "11110"
            , "10110"
            , "10111"
            , "10101"
            , "01111"
            , "00111"
            , "11100"
            , "10000"
            , "11001"
            , "00010"
            , "01010" 
            ]