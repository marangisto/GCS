module Level (main) where

import Data.Maybe
import Data.Either
import System.FilePath

type Pos = (Maybe Double, Maybe Double, Maybe Double)
type Feed = Maybe Double

data GCode
    = Percent
    | Fast Pos
    | Move Pos Feed
    | Comment String
    | Unhandled String
    deriving (Show)

parseLine :: String -> GCode
parseLine s@('(':_) = Comment $ filter (/='\r') s
parseLine s
    | [ "%" ] <- xs = Percent
    | ("G0" : ys) <- xs = Fast $ onlyCoords ys
    | ("G1" : ys) <- xs = uncurry Move $ coordsAndFeed ys
    | otherwise = Unhandled $ filter (/='\r') s
    where xs = words s

onlyCoords :: [String] -> Pos
onlyCoords = foldl g (Nothing, Nothing, Nothing)
    where g (_, y, z) ('X' : xs) = (Just $ read xs, y, z)
          g (x, _, z) ('Y' : xs) = (x, Just $ read xs, z)
          g (x, y, _) ('Z' : xs) = (x, y, Just $ read xs)

coordsAndFeed :: [String] -> (Pos, Feed)
coordsAndFeed = foldl g ((Nothing, Nothing, Nothing), Nothing)
    where g ((_, y, z), f) ('X' : xs) = ((Just $ read xs, y, z), f)
          g ((x, _, z), f) ('Y' : xs) = ((x, Just $ read xs, z), f)
          g ((x, y, _), f) ('Z' : xs) = ((x, y, Just $ read xs), f)
          g ((x, y, z), _) ('F' : xs) = ((x, y, z), Just $ read xs)

data State = State
    { pos   :: Pos
    , feed  :: Feed
    } deriving (Show)

expandCode :: [GCode] -> [GCode]
expandCode = tail . map snd . scanl (\(s,_) c -> advance s c) (s0, Percent)
    where s0 = State (Nothing, Nothing, Nothing) Nothing

advance :: State -> GCode -> (State, GCode)
advance s (Fast p) = let s' = updatePos p s in (s', Fast $ pos s')
advance s (Move p f) = let s' = updatePosFeed (p, f) s in (s', Move (pos s') (feed s'))
advance s c = (s, c)

updatePos :: Pos -> State -> State
updatePos (x', y', z') s@State{pos = (x, y, z)} = s
    { pos = (x' <|> x, y' <|> y, z' <|> z)
    }

updatePosFeed :: (Pos, Feed) -> State -> State
updatePosFeed ((x', y', z'), f') s@State{pos = (x, y, z), feed = f} = s
    { pos = (x' <|> x, y' <|> y, z' <|> z)
    , feed = f' <|> f
    }

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just x <|> _ = Just x
_ <|> Just y = Just y
_ <|> _ = Nothing

applyFeed :: Feed -> State -> State
applyFeed (Just f') s = s { feed = Just f' }
applyFeed _ s = s

to2D :: GCode -> Maybe (Double, Double)
to2D (Fast (Just x, Just y, _)) = Just (x, y)
to2D (Move (Just x, Just y, _) _) = Just (x, y)
to2D _ = Nothing

main :: IO ()
main = do
    let dir = "/home/marten/Desktop/samba/CNC"
        fn = "vco-bore-4mm"
    s <- readFile $ dir </> fn <.> "nc"
    let gc = expandCode . map parseLine $ lines s
    mapM_ print gc
    writeFile (dir </> fn <.> "csv") $ unlines $ map (\(x, y) -> show x ++ "," ++ show y) $ mapMaybe to2D gc

