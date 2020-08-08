module Main (GCode(..), parseGCode, main) where

import System.FilePath
import Text.Regex.Applicative
import Data.Maybe
import Data.Char hiding (Space)

data GCode
    = G Int
    | M Int
    | T Int
    | X Double
    | Y Double
    | Z Double
    | I Double
    | J Double
    | K Double
    | F Double
    | S Double
    | Comment String
    | Percent
    deriving Show

int :: RE Char Int
int = read <$> some (psym isDigit)

double :: RE Char Double
double = read <$> ((++) <$> sign <*> ((++) <$> integer <*> fraction))
    where integer = (++) <$> many (psym isDigit) <*> few (sym '.')
          fraction = some (psym isDigit)
          sign = few (sym '-')

space :: RE Char String
space = many $ psym isSpace

comment :: RE Char String
comment = filter (/='\r') <$> ((:) <$> sym '(' <*> many anySym)

gcode :: RE Char GCode
gcode = const G <$> sym 'G' <*> int
    <|> const M <$> sym 'M' <*> int
    <|> const T <$> sym 'T' <*> int
    <|> const X <$> sym 'X' <*> double
    <|> const Y <$> sym 'Y' <*> double
    <|> const Z <$> sym 'Z' <*> double
    <|> const I <$> sym 'I' <*> double
    <|> const J <$> sym 'J' <*> double
    <|> const K <$> sym 'K' <*> double
    <|> const F <$> sym 'F' <*> double
    <|> const S <$> sym 'S' <*> double
    <|> Comment <$> comment
    <|> Percent <$ sym '%'

block :: RE Char [GCode]
block = catMaybes <$> many ((Just <$> gcode) <|> (Nothing <$ space))

parseGCode :: [String] -> [Maybe [GCode]]
parseGCode = map (=~block)

main :: IO ()
main = do
    print $ parseGCode [ "X1", "Y1.4", "Z-2", "I-10.9" ]

