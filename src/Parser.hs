module Parser where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

intParser :: Parser Int
intParser = lexeme L.decimal

csvParser :: Parser [Int]
csvParser = do
    ints <- sepEndBy1 intParser (char ',') 
    pure ints

pairParser :: Parser (Int, Int)
pairParser = do
    i <- intParser
    char ','
    j <- intParser
    pure (i,j)
 

lineParser :: Parser a -> Parser [a]
lineParser parser = sepEndBy1 parser eol

readInput :: FilePath -> Parser a -> IO a
readInput path parser = do
    contents <- readFile path
    case runParser parser "" contents of
        Left _ -> error "Unable to parse input"
        Right input -> pure input
