module Day20 where

import Data.List
import Data.Vector ( (!) )
import qualified Data.Vector as V
import qualified Data.Map as M

type Coord = (Int, Int)
type Grid = M.Map Coord Char

printGrid :: Grid -> IO ()
printGrid grid =
    mapM_ putStrLn $ map (map snd) (grouped grid)
    where
        xs = map fst (M.keys grid)
        ys = map snd (M.keys grid)
        coords = [(x,y) | y <- [minimum ys .. maximum ys], x <- [minimum xs .. maximum xs]]
        grouped g = groupBy (\(x, _) (y, _) -> snd x == snd y) $ 
                          sortBy (\(x,_) (y,_) -> compare (snd x) (snd y)) (M.assocs g)

mkGrid :: [String] -> Grid
mkGrid rows =
    M.fromList $ zip coords (intercalate "" rows)
    where
        coords = [ (x, y) | y <- [0 .. (length rows) - 1], x <- [0 .. length rows - 1]]


getValue :: Char -> Coord -> Grid -> Char
getValue defVal coord g =
    M.findWithDefault defVal coord g

valToInt :: Char -> Int
valToInt c =
    if c == '.' then 0 else 1

neighbors :: Coord -> [Coord]
neighbors (x,y) = [(x + i, y + j) | j <- [-1,0,1], i <- [-1,0,1]]

computeIndex :: Char -> Coord -> Grid -> Int 
computeIndex v c g = 
    foldl (\x -> (+) (2*x)) 0 s
    where
        s = map (\x -> (valToInt (getValue v x g))) (neighbors c)

newPixel :: Char -> V.Vector Char -> Grid -> Coord -> Char
newPixel v alg g c = alg ! (computeIndex v c g)


enhance :: Int -> V.Vector Char -> Grid -> Grid
enhance step alg grid = 
    foldr (\c g-> M.insert c (newPixel val alg grid c) g) M.empty (M.keys grid <> borders)
    where
        (minX, minY) = fst $ M.findMin grid
        (maxX, maxY) = fst $ M.findMax grid
        borders = [(x,y) | y <- [minY-1 .. maxY+1], x <- [minX-1 .. maxX+1]]
        val = if even step then '.' else '#'
        -- val = '.'            -- This is for the test data

parseInput :: String -> (V.Vector Char, Grid)
parseInput s = (algorithm, grid)
    where
        ls = lines s
        algorithm = V.fromList $ head ls
        grid = mkGrid (drop 2 ls)  -- remove the algorithm and blank line from input

countOn :: Grid -> Int 
countOn grid = M.size $ M.filter (=='#') grid

part :: Int -> V.Vector Char -> Grid -> Int 
part end alg grid =
    countOn $ go 0 grid
    where
        go n g =
            if n == end
                then g
                else go (n+1) (enhance n alg g)

main :: IO ()
main = do
    contents <- readFile "data/day20-input.txt"
    let (alg, grid) = parseInput contents
    putStrLn $ "Part 1: " <> show (part 2 alg grid)
    putStrLn $ "Part2: " <> show (part 50 alg grid)

-- Test Data

testData = "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\n\
\#..#.\n\
\#....\n\
\##..#\n\
\..#..\n\
\..###\n"


