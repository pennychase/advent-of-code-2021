{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day2 where

import Control.Lens
import Control.Lens.Operators
import Data.List (foldl')
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

data Position
    = Position 
        { _depth :: Int 
        , _horiz :: Int 
        }
    deriving (Show, Eq)

makeLenses ''Position

data Course 
    = Course
        { _position :: Position
        , _aim :: Int 
        }
    deriving (Show, Eq)

makeLenses ''Course

data Direction = Forward | Up | Down
    deriving Show

data Move = Move Direction Int
    deriving Show

-- Original move1 using RecordWildcards and at patterns
-- move1 :: Move -> Position -> Position
-- move1 m pos@Position{ .. } =
--     case m of
--         Move Forward n -> pos { _horiz = _horiz + n }
--         Move Up n -> pos { _depth = _depth - n }
--         Move Down n -> pos { _depth = _depth + n }

-- Using lenses
-- +~ adds a value to the lens; -~ subtracts a value from the lens
move1 :: Move -> Position -> Position
move1 m pos =
    case m of
        Move Forward n -> pos & horiz +~ n
        Move Up n -> pos & depth -~ n
        Move Down n -> pos & depth +~ n

move2 :: Move -> Course -> Course
move2 m course =
    case m of
        Move Forward n -> course & position . horiz +~ n & position . depth +~ (course^.aim * n)
        Move Up n -> course & aim -~ n 
        Move Down n -> course & aim +~ n


part1 :: [Move] -> Int 
part1 moves = pos ^. depth * pos ^. horiz 
    where
        pos = foldr move1 (Position 0 0) moves

part2 :: [Move] -> Int 
part2 moves = (course ^. position . depth) * (course ^. position . horiz)
    where
        course = foldl' (flip move2) (Course (Position 0 0) 0) moves

-- Parser
type Parser a = Parsec Void String a

forwardParser :: Parser Direction
forwardParser = do
    string' "forward"
    pure $ Forward

upParser :: Parser Direction
upParser = do
    string' "up"
    pure $ Up

downParser :: Parser Direction
downParser = do
    string' "down"
    pure $  Down

directionParser :: Parser Direction
directionParser = do
    try forwardParser <|> upParser <|> downParser

integerParser :: Parser String 
integerParser = try $ some digitChar

moveParser :: Parser Move
moveParser = do
    dir <- directionParser
    hspace 
    num <- integerParser
    many eol
    pure $ Move dir (read num :: Int)

parseMoves :: Parser [Move]
parseMoves = do
    some moveParser


main :: IO ()
main = do
    contents <- readFile "./data/day2-input.txt"
    case runParser parseMoves "" contents of
        Left err -> putStrLn "Unable to parse input"
        Right moves -> do
            putStrLn $ "Part 1: " <> show (part1 moves)
            putStrLn $ "Part 2: " <> show (part2 moves)

-- Test Data
testData :: String
testData = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"
