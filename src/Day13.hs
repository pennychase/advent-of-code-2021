module Day13 where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M

import Parser

data On = On Bool 
    deriving (Eq, Ord)

instance Show On where
    show (On True) = "#"
    show (On False) = "."

type Grid = Map (Int, Int) On

initializeGrid :: [(Int, Int)] -> Grid
initializeGrid points =
    foldr ((flip M.insert) (On True)) initial points
    where
        initial = M.fromList (zip coords (cycle [On False]))
        xs = sort $ map fst points
        ys = sort $ map snd points
        coords = [(x,y) | y <- ys, x <- xs]

printGrid :: Grid -> IO ()
printGrid grid =
    mapM_ (putStrLn. showLine) (grouped grid)
    where
        grouped g = groupBy (\(x, _) (y, _) -> snd x == snd y) $ 
                          sortBy (\(x,_) (y,_) -> compare (snd x) (snd y)) (M.assocs g)
        showLine = concatMap (show . snd)



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
\9,0\n"
