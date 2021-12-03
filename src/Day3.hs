module Day3 where

import Data.List (transpose)

type LCB = Int 
type MCB = Int 
type Epsilon = Int 
type Gamma = Int

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

-- FInd least and most common bits in all bit positions
leastAndMostCommonBits :: [String] -> [(LCB, MCB)]
leastAndMostCommonBits bits = map (leastAndMostCommonBit . countBits) bits

-- Use zipWith to mltiply the bits by powers of 2, and sum the result
-- Since "iterate (2*) 1" computers the powers of 2 from 1, 2, 4, ...
-- reverse the bits to multiply in the right order
bitsToInt :: [Int] -> Int 
bitsToInt bits = sum $ zipWith (*) (reverse bits) (iterate (2*) 1)

computeEpsilonGamma :: [String] -> (Epsilon, Gamma)
computeEpsilonGamma bits =(epsilon, gamma)
    where
        lcbAndmcb = leastAndMostCommonBits $ transpose bits
        epsilon = bitsToInt $ map fst lcbAndmcb
        gamma = bitsToInt $ map snd lcbAndmcb

part1 :: String -> Int 
part1 content = epsilon * gamma
    where
        (epsilon, gamma) = computeEpsilonGamma $ lines content

main :: IO ()
main = do
    content <- readFile "./data/day3-input.txt"
    putStrLn $ "Part 1: " <> show (part1 content)

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