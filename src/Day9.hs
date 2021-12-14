module Day9 where

import Data.Char
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V
import Data.Bits (Bits(xor))

type Grid = Vector (Vector Int)

axes :: Grid -> ([Int], [Int])
axes grid = ( [0 .. V.length grid - 1], [0 .. V.length (grid ! 0) - 1] )

getCell :: Grid -> (Int, Int )-> Maybe Int 
getCell grid (i,j) = grid !? i >>= (!? j)

getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
 
getNeighborsVals :: Grid -> (Int, Int) -> [Int]
getNeighborsVals grid pt = mapMaybe (\x -> getCell grid x) (getNeighbors pt)


-- Part 1

isMin :: Grid -> (Int, Int) -> Maybe (Int, Int)
isMin grid p =
    case all (curVal<) (getNeighborsVals grid p )of 
        False -> Nothing 
        True -> pure p
    where
        curVal = fromJust $ getCell grid p

allMins :: Grid -> [(Int,Int)]
allMins grid = 
    catMaybes $ map (\(i, j) -> isMin grid (i, j))
                    [(i, j) | i <- xs, j <- ys]
    where (xs, ys) = axes grid

riskLevel :: Grid -> Int 
riskLevel grid = sum $ mapMaybe (\(i, j) -> (+1) <$> (getCell grid (i,j))) $ allMins grid  

part1 :: Grid -> Int 
part1 = riskLevel 

-- Part 2

findBasin :: Grid -> [(Int, Int)] -> Set (Int, Int)
findBasin grid coords = findBasin' coords S.empty
    where
        findBasin' [] accum = accum
        findBasin' ((i,j):cs) accum =
            case S.member (i,j) accum of
                True -> findBasin' cs accum
                False -> case getCell grid (i, j) of
                    Nothing -> findBasin' cs accum
                    Just 9 -> findBasin' cs accum
                    _ -> findBasin' (getNeighbors (i, j) <> cs) (S.insert (i,j) accum)

findBasins :: Grid -> [Set (Int, Int)]
findBasins grid =
    map (\p -> findBasin grid [p]) (allMins grid)

part2 :: Grid -> Int 
part2 grid =
    product $ take 3 $ sortBy (flip compare) (map S.size (findBasins grid))   
        

parseInput :: String -> Grid
parseInput content =
    V.fromList $ map (V.fromList . (map digitToInt)) (lines content)


main :: IO ()
main = do
    contents <- readFile "data/day9-input.txt"
    let grid = parseInput contents
    putStrLn $ "Part 1: " <> show (part1 grid)
    putStrLn $ "Part 2: " <> show (part2 grid)

-- Test Data
testData :: Grid
testData = V.fromList
    [ V.fromList [2,1,9,9,9,4,3,2,1,0] 
    , V.fromList [3,9,8,7,8,9,4,9,2,1]
    , V.fromList [9,8,5,6,7,8,9,8,9,2]
    , V.fromList [8,7,6,7,8,9,6,7,8,9]
    , V.fromList [9,8,9,9,9,6,5,6,7,8]
    ]