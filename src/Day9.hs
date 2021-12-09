module Day9 where

import Data.Char
import Data.Maybe
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V

type Grid = Vector (Vector Int)

bounds :: Grid -> (Int, Int)
bounds grid = (V.length grid - 1, V.length (grid ! 0) - 1)

getCell :: Grid -> Int -> Int -> Maybe Int 
getCell grid i j = grid !? i >>= (!? j)

getNeighbors :: Grid -> Int -> Int -> [Int]
getNeighbors grid i j = catMaybes
    [ getCell grid (i - 1) j
    , getCell grid (i + 1) j
    , getCell grid i (j - 1)
    , getCell grid i (j + 1)
    ]

isMin :: Grid -> Int -> Int -> Maybe Int
isMin grid i j =
    case all (curVal<) (getNeighbors grid i j) of 
        False -> Nothing 
        True -> pure curVal
    where
        curVal = fromJust $ getCell grid i j

allMins :: Grid -> [Int]
allMins grid = 
    catMaybes $ map (\(i, j) -> isMin grid i j) 
                    [(i, j) | i <- [0 .. fst (bounds grid)], j <- [0 .. snd (bounds grid)]]

riskLevel :: Grid -> Int 
riskLevel grid = sum $ map (+1) (allMins grid)

parseInput :: String -> Grid
parseInput content =
    V.fromList $ map (V.fromList . (map digitToInt)) (lines content)

part1 :: String -> Int 
part1 = riskLevel . parseInput

main :: IO ()
main = do
    contents <- readFile "data/day9-input.txt"
    putStrLn $ "Part 1: " <> show (part1 contents)

grid :: Grid
grid = V.fromList
    [ V.fromList [2,1,9,9,9,4,3,2,1,0] 
    , V.fromList [3,9,8,7,8,9,4,9,2,1]
    , V.fromList [9,8,5,6,7,8,9,8,9,2]
    , V.fromList [8,7,6,7,8,9,6,7,8,9]
    , V.fromList [9,8,9,9,9,6,5,6,7,8]
    ]