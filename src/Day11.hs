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

data SimState = SimState
    { grid :: Grid
    , flashes :: Int 
    }

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

        
updateCell :: Grid -> Coord -> State (Int, Int) Grid
updateCell grid coord  = do
    (steps, flashes) <- get
    let xs = map (newVal <$>) (findNeighbors coord grid)
    let grid' = foldl (\m (k, v) -> M.insert k v m) (M.insert coord 0 grid) xs
    put (steps, flashes + 1)
    pure grid'


step :: Grid -> State (Int, Int) Grid
step grid = do
    (steps, flashes) <- get
    put (steps + 1, flashes)
    go (M.map (+1) grid)
    where
        go :: Grid -> State (Int, Int) Grid
        go g =
            let nines = M.filter (> 9) g
            in
                if M.null nines 
                    then pure g
                    else do
                        g' <- foldM updateCell g (M.keys nines)
                        go g'

initialize :: String -> Grid
initialize input = grid
    where
        (nrow, ncol, vals) = parseInput input
        grid = mkGrid nrow ncol vals

solve :: Int -> Grid -> State (Int, Int) Grid
solve n grid = do
    if n == 0 then pure grid
        else do
            grid' <- step grid
            solve (n - 1) grid'

solve2 :: Grid -> State (Int, Int) Grid
solve2 grid = do
    if allFlash then pure grid
        else do
            grid' <- step grid
            solve2 grid'
    where
        allFlash = M.size (M.filter (== 0) grid) == M.size grid

part1 :: String -> (Int, Int)
part1 input =
    execState (solve 100 grid) (0, 0)
    where
        grid = initialize input

part2 :: String -> (Int, Int)
part2 input =
    execState (solve2 grid) (0, 0)
    where
        grid = initialize input
                    

parseInput :: String -> (Int, Int, [Int])
parseInput input = (nrow, ncol, map digitToInt (concat ls))
    where
        ls = lines input
        nrow = length ls - 1
        ncol = length (head ls) - 1

main :: IO ()
main = do
    contents <- readFile "data/day11-input.txt"
    putStrLn $ "Part 1: " <> show (part1 contents)
    putStrLn $ "Part 2: " <> show (part2 contents)

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