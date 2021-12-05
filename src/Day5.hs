module Day5 where

import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Point = (Int, Int)
type Line = (Point, Point)

horizP :: Line -> Bool 
horizP (p1, p2) = snd p1 == snd p2

vertP :: Line -> Bool 
vertP (p1, p2) = fst p1 == fst p2

pointsInLine :: Line -> Set Point
pointsInLine line@(p1, p2) 
    | horizP line = S.fromList $ zip (genCoord (fst p1) (fst p2)) (cycle [snd p1])
    | vertP line = S.fromList $  zip (cycle [fst p1]) (genCoord (snd p1) (snd p2))
    | otherwise = S.fromList $ zip (genCoord (fst p1) (fst p2)) (genCoord (snd p1) (snd p2))
    where
        genCoord c1 c2 
            | c1 < c2 = [c1, c1 + 1 .. c2]
            | c1 >= c2 = [c1, c1 - 1 .. c2]


countTrue :: Enum a => [a] -> Int
countTrue list = sum $ map fromEnum list   

pointsInCommon :: [Set Point] -> Int
pointsInCommon linesPts =
    -- count how many points occur in 2 or more lines
    length $ filter (>= 2) overlaps
    where
        -- all the points appearing in at least one line
        allPts = S.elems $ foldr S.union S.empty linesPts 
        -- for each point, find the lines it's in, and count how many
        overlaps = map countTrue $ map (\p -> map (S.member p) linesPts) allPts

part1 :: [Line] -> Int
part1 lines =
    pointsInCommon linesPts
    where
        linesPts = map pointsInLine $ (filter horizP lines) <> (filter vertP lines)

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