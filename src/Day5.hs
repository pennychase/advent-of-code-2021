module Day5 where

import Data.List
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import qualified Control.Lens as MS

type Point = (Int, Int)
type Line = (Point, Point)

horizP :: Line -> Bool 
horizP (p1, p2) = snd p1 == snd p2

vertP :: Line -> Bool 
vertP (p1, p2) = fst p1 == fst p2

pointsInLine :: Line -> MultiSet Point
pointsInLine line@(p1, p2) 
    | horizP line = MS.fromList $ zip (genCoord (fst p1) (fst p2)) (cycle [snd p1])
    | vertP line = MS.fromList $ zip (cycle [fst p1]) (genCoord (snd p1) (snd p2))
    | otherwise = MS.fromList $ zip (genCoord (fst p1) (fst p2)) (genCoord (snd p1) (snd p2))
    where
        genCoord c1 c2 
            | c1 < c2 = [c1, c1 + 1 .. c2]
            | c1 >= c2 = [c1, c1 - 1 .. c2]

pointsInCommon :: [MultiSet Point] -> Int
pointsInCommon lns =
    MS.distinctSize $ MS.filter (\pt -> (MS.occur pt points) >= 2) points
    where
        points = MS.unions lns

part1 :: [Line] -> Int
part1 lines =
    pointsInCommon $ map pointsInLine $ filter horizP lines <> filter vertP lines

part2 :: [Line] -> Int
part2 lines = pointsInCommon (map pointsInLine lines)

main :: IO ()
main = do
    contents <- readFile "./data/day5-input.txt"
    case runParser parseLines "" contents of
        Left err -> putStrLn "Unable to parse input"
        Right input -> do
            putStrLn $ "Part 1: " <> show (part1 input)
            putStrLn $ "Part 2: " <> show (part2 input)

-- Parser
type Parser a = Parsec Void String a

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

pointParser :: Parser Point
pointParser = do
    xcoord <- L.decimal
    char ','
    ycoord <- L.decimal
    pure (xcoord, ycoord)

lineParser :: Parser Line
lineParser = do
    pt1 <- pointParser
    try (string " -> " <|> string " <- ")
    pt2 <- pointParser
    pure (pt1, pt2)

parseLines :: Parser [Line]
parseLines = sepEndBy1 lineParser eol

-- Test Data
ls :: [Line]
ls = 
   [ ((0,9), (5,9))
   , ((8,0), (0,8))
   , ((9,4), (3,4))
   , ((2,2), (2,1))
   , ((7,0), (7,4))
   , ((6,4), (2,0))
   , ((0,9), (2,9))
   , ((3,4), (1,4))
   , ((0,0), (8,8))
   , ((5,5), (8,2))
   ]