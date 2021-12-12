module Day10 where

import Data.List
import Data.List.NonEmpty ( NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
 
type Parser = Parsec Void String

-- Parser

parseBraces :: Parser ()
parseBraces = 
    many (parens <|> braces <|> angles <|> brackets) >> return ()
    where
        parens    = between (string' "(") (string' ")") parseBraces
        braces    = between (string' "{") (string' "}") parseBraces
        angles    = between (string' "<") (string' ">") parseBraces
        brackets  = between (string' "[") (string' "]") parseBraces

-- Part 1

processLine :: String -> Maybe Char
processLine str =
    case runParser parseBraces "" str of
        Right _ -> Nothing 
        Left (ParseErrorBundle ((TrivialError pos err _) :| _) _) -> getToken str pos err

getToken :: String -> Int -> Maybe (ErrorItem (Token String)) -> Maybe Char
getToken str n err =
    case err of
        Just EndOfInput -> Nothing
        _ -> pure (str !! n)

cost :: Char -> Int
cost c = 
    case c of
        ')' -> 3
        ']' -> 57
        '}' -> 1197
        '>' -> 25137

part1 :: [String] -> Int 
part1 lines =
    sum . map cost $ mapMaybe processLine lines


-- Part 2

endBraces = S.fromList 
    [ Tokens (')' :| "")
    , Tokens (']' :| "")
    , Tokens ('>' :| "")
    , Tokens ('}' :| "")
    ]

processLine2 :: String -> Maybe String
processLine2 str = go str ""
    where 
        go str acc = 
            case runParser parseBraces "" str of
                Right _ -> pure acc 
                Left (ParseErrorBundle ((TrivialError _ err toks) :| _) _) -> 
                    case getToken2 err toks of
                        Nothing -> Nothing 
                        Just s -> go (str ++ s) (s ++ acc)

getToken2 :: Maybe (ErrorItem (Token String)) -> Set (ErrorItem (Token String)) -> Maybe String 
getToken2 err toks =
    case err of
        Just EndOfInput -> Just [tok]
        _ -> Nothing
    where
        Tokens (tok :| _) = head $ S.toList $ S.intersection endBraces toks

cost2 :: Char -> Int
cost2 c =
    case c of 
        ')' -> 1
        ']' -> 2
        '}' -> 3
        '>' -> 4

score :: String -> Int 
score str = foldr (\x a -> cost2 x + (5 * a)) 0 str

median :: Ord a => [a] -> a
median xs = head (drop (length xs `div` 2) (sort xs))

part2 :: [String] -> Int 
part2 lines =
    median $ map score $ mapMaybe processLine2 lines

main :: IO ()
main = do
    content <- readFile "data/day10-input.txt"
    let strs = lines content
    putStrLn $ "Part 1: " <> show (part1 strs)
    putStrLn $ "Part 2: " <> show (part2 strs)

-- Test Data
testData :: String
testData = "[({(<(())[]>[[{[]{<()<>>\n\
\[(()[<>])]({[<{<<[]>>(\n\
\{([(<{}[<>[]}>{[]{[(<()>\n\
\(((({<>}<{<{<>}{[]{[]{}\n\
\[[<[([]))<([[{}[[()]]]\n\
\[{[{({}]{}}([{[{{{}}([]\n\
\{<[[]]>}<{[{[{[]{()[[[]\n\
\[<(<(<(<{}))><([]([]()\n\
\<{([([[(<>()){}]>(<<{{\n\
\<{([{{}}[<[[[<>{}]]]>[]]"


