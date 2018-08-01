module Main (GCode(..), parseGCode, main) where

import System.FilePath
import Text.Regex.Applicative
import Data.Foldable
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
double = read <$> (combine <$> sign <*> integer <*> fraction)
    where combine x y z = x ++ y ++ z
          integer = (++) <$> many (psym isDigit) <*> few (sym '.')
          fraction = some (psym isDigit)
          sign = few (sym '-')

space :: RE Char String
space = many $ psym isSpace

comment :: RE Char String
comment = filter (/='\r') <$> ((:) <$> sym '(' <*> many anySym)

gcode :: RE Char GCode
gcode = asum
    [ G <$ sym 'G' <*> int
    , M <$ sym 'M' <*> int
    , T <$ sym 'T' <*> int
    , X <$ sym 'X' <*> double
    , Y <$ sym 'Y' <*> double
    , Z <$ sym 'Z' <*> double
    , I <$ sym 'I' <*> double
    , J <$ sym 'J' <*> double
    , K <$ sym 'K' <*> double
    , F <$ sym 'F' <*> double
    , S <$ sym 'S' <*> double
    , Comment <$> comment
    , Percent <$ sym '%'
    ]

block :: RE Char [GCode]
block = catMaybes <$> many ((Just <$> gcode) <|> (Nothing <$ space))

parseGCode :: [String] -> [Maybe [GCode]]
parseGCode = map (=~block)

main :: IO ()
main = do
    let dir = "/home/marten/Desktop/samba/CNC"
        fn = "vco-bore-4mm"
    xs <- lines <$> readFile (dir </> fn <.> "nc")
    mapM_ print $ parseGCode xs

