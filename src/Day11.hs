{-# LANGUAGE TupleSections #-}

module Day11 where

import Control.Monad.State
import Data.Char
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

type Coord = (Int, Int)
type Grid = Map Coord Int

newVal :: Int -> Int 
newVal n =
    if n == 0 then 0 else n + 1

mkGrid :: Int -> Int -> [Int] -> Grid
mkGrid nrow ncol vals = M.fromList $ zip [(i,j) | i <- [0..nrow], j <- [0..ncol]] vals

printGrid :: Grid -> IO ()
printGrid grid = do
    putStr $ unlines $ map (concatMap show) $ chunksOf (n+1) $ M.elems grid
    where
        ((n, _), _) = M.findMax grid

neighbors :: Coord -> [Coord]
neighbors (i, j) =
    [ (i-1, j-1)
    , (i-1, j)
    , (i-1, j+1)
    , (i, j-1)
    , (i, j+1)
    , (i+1, j-1)
    , (i+1, j)
    , (i+1, j+1)
    ]


findNeighbors :: Coord -> Grid -> [(Coord, Int)]
findNeighbors coord grid =
    mapMaybe (\c -> (c,) <$> M.lookup c grid) (neighbors coord)

        
updateCell :: Coord -> Grid -> Grid
updateCell coord grid =
    foldl (\m (k, v) -> M.insert k v m) (M.insert coord 0 grid) xs
    where 
        xs = map (newVal <$>) (findNeighbors coord grid)

step :: Grid -> Grid
step grid =
    go addedOne
    where
        addedOne = M.map (+1) grid
        go g =
            let
                nines = M.filter (> 9) g
            in
                if M.null nines 
                    then g
                    else go $ foldr updateCell g (M.keys nines)


part1 :: Int -> String -> Grid
part1 n input =
    (iterate step grid) !! n
    where
        (nrow, ncol, vals) = parseInput input
        grid = mkGrid nrow ncol vals
            

parseInput :: String -> (Int, Int, [Int])
parseInput input = (nrow, ncol, map digitToInt (concat ls))
    where
        ls = lines input
        nrow = length ls - 1
        ncol = length (head ls) - 1

main :: IO ()
main = do
    contents <- readFile "data/day11-input.txt"
    putStrLn "Part 1: " <> show (part1 contents)

-- Test Data
testData1 :: String 
testData1 =
    "11111\n\
\19991\n\
\19191\n\
\19991\n\
\11111\n"

testData :: String 
testData =
    "5483143223\n\
\2745854711\n\ 
\5264556173\n\
\6141336146\n\
\6357385478\n\
\4167524645\n\
\2176841721\n\
\6882881134\n\
\4846848554\n\
\5283751526\n"