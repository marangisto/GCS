{-# LANGUAGE RecordWildCards #-}
module Level (main) where

import Data.Maybe
import Data.Either

type Pos = (Maybe Double, Maybe Double, Maybe Double)
type Feed = Maybe Double

data GCode
    = Percent
    | Fast Pos
    | Move Pos Feed
    | Comment String
    | Unhandled
    deriving (Show)

parseLine :: String -> GCode
parseLine s@('(':_) = Comment $ filter (/='\r') s
parseLine s
    | [ "%" ] <- xs = Percent
    | ("G0" : ys) <- xs = Fast $ onlyCoords' ys
    | ("G1" : ys) <- xs = uncurry Move $ coordsAndFeed' ys
    | otherwise = Unhandled
    where xs = words s

onlyCoords' :: [String] -> Pos
onlyCoords' = foldl g (Nothing, Nothing, Nothing)
    where g (_, y, z) ('X' : xs) = (Just $ read xs, y, z)
          g (x, _, z) ('Y' : xs) = (x, Just $ read xs, z)
          g (x, y, _) ('Z' : xs) = (x, y, Just $ read xs)

coordsAndFeed' :: [String] -> (Pos, Feed)
coordsAndFeed' = foldl g ((Nothing, Nothing, Nothing), Nothing)
    where g ((_, y, z), f) ('X' : xs) = ((Just $ read xs, y, z), f)
          g ((x, _, z), f) ('Y' : xs) = ((x, Just $ read xs, z), f)
          g ((x, y, _), f) ('Z' : xs) = ((x, y, Just $ read xs), f)
          g ((x, y, z), _) ('F' : xs) = ((x, y, z), Just $ read xs)

data State = State
    { x :: Maybe Double
    , y :: Maybe Double
    , z :: Maybe Double
    , f :: Maybe Double
    } deriving (Show)

expandCode :: [GCode] -> [GCode]
expandCode = tail . map snd . scanl (\(s,_) c -> advance s c) (s0, Percent)
    where s0 = State Nothing Nothing Nothing Nothing

advance :: State -> GCode -> (State, GCode)
advance s (Fast pos) = let s' = updatePos pos s in (s', Fast $ getPos s')
advance s (Move pos f) = let s' = updatePosFeed (pos, f) s in (s', Move (getPos s') (getFeed s'))
advance s c = (s, c)

updatePos :: Pos -> State -> State
updatePos (x', y', z') s@State{..} = s
    { x = x' <|> x
    , y = y' <|> y
    , z = z' <|> z
    }

updatePosFeed :: (Pos, Feed) -> State -> State
updatePosFeed ((x', y', z'), f') s@State{..} = s
    { x = x' <|> x
    , y = y' <|> y
    , z = z' <|> z
    , f = f' <|> f
    }

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just x <|> _ = Just x
_ <|> Just y = Just y
_ <|> _ = Nothing

getPos :: State -> Pos
getPos State{..} = (x, y, z)

applyFeed :: Feed -> State -> State
applyFeed (Just f') s = s { f = Just f' }
applyFeed _ s = s

getFeed :: State -> Feed
getFeed State{..} = f

main :: IO ()
main = do
    s <- readFile "/home/marten/Desktop/samba/CNC/vco-contour-4mm.nc"
    mapM_ print . expandCode . map parseLine $ lines s

