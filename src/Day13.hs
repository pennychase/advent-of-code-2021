module Day13 where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char

import Parser

newtype On = On Bool
    deriving (Eq, Ord)

instance Show On where
    show (On True) = "#"
    show (On False) = "."

combineOn :: On -> On -> On
combineOn (On o1) (On o2) = On (o1 || o2)

type Grid = Map (Int, Int) On

data Dim = XDim | YDim
    deriving (Show, Eq)

data Fold = Fold Dim Int 
    deriving (Show, Eq)

initializeGrid :: [(Int, Int)] -> Grid
initializeGrid points = fillGrid $ M.fromList $ zip points (cycle [On True])

fillGrid :: Grid -> Grid
fillGrid grid =
    foldr ((flip M.insert) (On True)) initial (M.keys grid)
    where
        initial = M.fromList (zip coords (cycle [On False]))
        xs = map fst (M.keys grid)
        ys = map snd (M.keys grid)
        coords = [(x,y) | y <- [minimum ys .. maximum ys], x <- [minimum xs .. maximum xs]]

printGrid :: Grid -> IO ()
printGrid grid =
    mapM_ (putStrLn . showLine) (grouped grid)
    where
        xs = map fst (M.keys grid)
        ys = map snd (M.keys grid)
        coords = [(x,y) | y <- [minimum ys .. maximum ys], x <- [minimum xs .. maximum xs]]
        grouped g = groupBy (\(x, _) (y, _) -> snd x == snd y) $ 
                          sortBy (\(x,_) (y,_) -> compare (snd x) (snd y)) (M.assocs g)
        showLine = concatMap (show . snd)

foldAlongX :: Int -> Grid -> Grid
foldAlongX x grid =
    M.unionWith combineOn left (M.mapKeys (\(i,j) -> (x - i + x, j)) right)
    where
        grid' = M.filterWithKey (\k _ -> fst k /= x) grid
        (right, left) = M.partitionWithKey (\k _ -> fst k > x) grid'


foldAlongY :: Int -> Grid -> Grid
foldAlongY y grid =
    M.unionWith combineOn upper (M.mapKeys (\(i,j) -> (i, y - j + y)) lower)
    where
        grid' = M.filterWithKey (\k _ -> snd k /= y) grid
        (lower, upper) = M.partitionWithKey (\k _ -> snd k > y) grid'

                             
foldAlong :: Dim -> Int -> Grid -> Grid 
foldAlong dim ln grid =
    case dim of
        XDim -> foldAlongX ln grid
        YDim -> foldAlongY ln grid

countOn :: Grid -> Int 
countOn g = M.size $ M.filter (== On True) g

-- Parsing
foldParser :: Parser Fold
foldParser = do
    string "fold along "
    dim <- char 'x' <|> char 'y'
    char '='
    num <- intParser
    eol
    case dim of
        'x' -> pure (Fold XDim num)
        'y' -> pure (Fold YDim num)

parseData :: Parser ([(Int, Int)], [Fold])
parseData = do
    pairs <- lineParser pairParser 
    eol 
    folds <- many foldParser
    pure (pairs, folds)

part1 :: Fold -> Grid -> Int 
part1 (Fold dim ln) g = countOn $ foldAlong dim ln g

part2 :: [Fold] -> Grid -> Grid
part2 folds grid = foldl (\g (Fold dim ln) -> foldAlong dim ln g) grid folds

main :: IO ()
main = do
    (coords, folds) <- readInput "data/day13-input.txt" parseData
    let grid = initializeGrid coords
    putStrLn $ "Part 1: " <> show (part1 (head folds) grid)
    putStrLn "Part 2:"
    printGrid (part2 folds grid)


-- Test Data
sample = "6,10\n\
\0,14\n\
\9,10\n\
\0,3\n\
\10,4\n\
\4,11\n\
\6,0\n\
\6,12\n\
\4,1\n\
\0,13\n\
\10,12\n\
\3,4\n\
\3,0\n\
\8,4\n\
\1,10\n\
\2,14\n\
\8,10\n\
\9,0\n\n\
\fold along y=7\n\
\fold along x=5\n"
