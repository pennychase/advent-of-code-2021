{-# LANGUAGE RecordWildCards #-}

module Day4 where

import Control.Lens
import Control.Lens.Operators
import Data.List
import Data.Maybe
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void
import GHC.Conc (numSparks)

newtype Board a = Board { board :: [[a]] }
    deriving Show

data Game = Game [Int] [Board Int]
    deriving Show

data Bingo =
    Bingo { values :: Board Int 
          , marked :: Board Bool
          }
        deriving Show

makeBingo :: Board Int -> Bingo
makeBingo b = Bingo { values = b, marked = emptyMarked rows cols}
    where
        rows = length (board b)
        cols = length (head (board b))

emptyMarked :: Int -> Int -> Board Bool 
emptyMarked nrows ncols = Board { board = replicate nrows row }
    where
        row = replicate nrows False


findInBoard :: Int -> Board Int -> Maybe (Int, Int)
findInBoard n (Board board) = ret
    where
        result = map (elemIndex n) board
        ret = case catMaybes result of
                [] -> Nothing 
                [col] -> let 
                            Just row = elemIndex (Just col) result
                         in pure (row, col)
                _ -> Nothing    -- should never reach this since numbers are unique on bingo cards

markBoard :: (Int, Int) -> Board Bool -> Board Bool 
markBoard (row, col) b = Board { board = (board b) & element row . element col .~ True }

updateBingo :: Int -> Bingo -> Bingo
updateBingo n bingo =
    case findInBoard n (values bingo) of
        Nothing -> bingo
        Just indices -> bingo { marked = markBoard indices (marked bingo) }

hasBingo :: Bingo -> Bool 
hasBingo bingo = 
        allMarked card || allMarked (transpose card)
    where
        card = board (marked bingo)
        allMarked squares = any and squares

score :: Int -> Bingo -> Int 
score n bingo =
    n * (sum $ map fst $ filter (not . snd) zipped)
    where
        zipped = zip (concat . board $ values bingo) (concat . board $ marked bingo)

game :: [Int] -> [Bingo] -> Maybe Int 
game [] _ = Nothing
game (n:ns) bs =
    let newBs = map (updateBingo n) bs
    in case elemIndex True (map hasBingo newBs) of
        Nothing -> game ns newBs
        Just i -> pure $ score n (newBs !! i)

-- For part 2 we have to find the last board to bingo. So we create a running list of scores
-- and the recursion now involves the boards as well as the draws. Each round we need to find
-- all the bingos, score them, and recurse after removing those boards.
game2 :: [Int] -> [Bingo] -> [Int]
game2 [] _ = []
game2 _ [] = []
game2 (n:ns) bs =
    let newBs = map (updateBingo n) bs
    in case elemIndices True (map hasBingo newBs) of
        [] -> game2 ns newBs
        is -> (map (sc newBs) is) ++ (game2 ns (del is newBs))
    where
        sc nbs i = score n (nbs !! i)
        del is nbs= snd . unzip $ filter (not . (`elem` is) . fst) (zip [0 ..] nbs)

part1 :: Game -> Maybe Int 
part1 (Game draws boards) = 
    game draws bingos
    where
        bingos = map makeBingo boards

part2 :: Game -> Int
part2 (Game draws boards) =
    last $ game2 draws bingos
    where
        bingos = map makeBingo boards

-- Parser
type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

-- Needed to use lexeme because some input lines started with a space (for alignment)
-- Parser in Day2 only removed trailing spaces
intParser :: Parser Int
intParser = do
    num <- lexeme L.decimal
    pure num

rowParser :: Parser [Int]
rowParser = do
    hspace
    nums <- some intParser
    eol 
    pure nums

boardParser :: Parser (Board Int)
boardParser = do
    rows <- some rowParser
    many eol
    pure $ Board rows

drawsParser :: Parser [Int]
drawsParser = do
    nums <- L.decimal `sepBy` char ','
    many eol 
    pure nums

parseBingo :: Parser Game
parseBingo = do
    draws <- drawsParser
    boards <- some boardParser
    pure $ Game draws boards

-- Main

main :: IO ()
main = do
    contents <- readFile "./data/day4-input.txt"
    case runParser parseBingo "" contents of
        Left err -> putStrLn "Unable to parse input"
        Right game -> do
            putStrLn $ "Part 1: " <> show (part1 game)
            putStrLn $ "Part 2: " <> show (part2 game)

-- Test Data
b1 = Bingo { values =  Board 
                { board = 
                    [ [22, 13, 17, 11,  0]
                    , [ 8,  2, 23,  4, 24]
                    , [21,  9, 14, 16,  7]
                    , [ 6, 10,  3, 18,  5]
                    , [ 1, 12, 20, 15, 19]
                    ]
                }
            , marked = emptyMarked 5 5
      }

b2 = Bingo { values = Board
                { board = 
                    [ [ 3, 15,  0,  2, 22]
                    , [ 9, 18, 13, 17,  5]
                    , [19,  8,  7, 25, 23]
                    , [20, 11, 10, 24,  4]
                    , [14, 21, 16, 12,  6]
                    ]
                }
            , marked = emptyMarked 5 5
        }

b3 = Bingo { values = Board
                { board =
                    [ [14, 21, 17, 24,  4]
                    , [10, 16, 15,  9, 19]
                    , [18,  8, 23, 26, 20]
                    , [22, 11, 13,  6,  5]
                    , [ 2,  0, 12,  3,  7]
                    ]
                }                                                                  
            , marked = emptyMarked 5 5
}

