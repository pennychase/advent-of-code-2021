{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

data Position
    = Position 
        { _depth :: Int 
        , _horiz :: Int 
        }
    deriving (Show, Eq)

data Course 
    = Course
        { _position :: Position
        , _aim :: Int 
        }
    deriving (Show, Eq)

data Direction = Forward | Up | Down
    deriving Show

data Move = Move Direction Int
    deriving Show

move1 :: Move -> Position -> Position
move1 m pos@Position{ .. } =
    case m of
        Move Forward n -> pos { _horiz = _horiz + n }
        Move Up n -> pos { _depth = _depth - n }
        Move Down n -> pos { _depth = _depth + n }

-- move2 :: Move -> Course -> Course
-- move2 m = course@Course { .. }
--     case m of
--         Move Forward n -> course { position = Position { horiz = horiz + x, depth = aim * n }}
--         Move Up n -> course { _aim = _aim - n }
--         Move Down n -> course { _aim  = _aim +  n }


part1 :: [Move] -> Int 
part1 moves = _depth pos * _horiz pos
    where
        pos = foldr move1 (Position 0 0) moves

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
        Right moves -> putStrLn $ "Part 1: " <> show (part1 moves)

