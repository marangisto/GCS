{-# LANGUAGE RecordWildCards #-}
module Level (main) where

import Data.Maybe
import Data.Either

type Pos = Maybe Double
type Feed = Maybe Double

data Coord = X Double | Y Double | Z Double deriving (Show)

data GCode
    = Percent
    | Fast [Coord]
    | Move [Coord] Feed
    | Comment String
    | Unhandled
    deriving (Show)

parseLine :: String -> GCode
parseLine s@('(':_) = Comment $ filter (/='\r') s
parseLine s
    | [ "%" ] <- xs = Percent
    | ("G0" : ys) <- xs = Fast $ onlyCoords ys
    | ("G1" : ys) <- xs = uncurry Move $ coordsAndFeed ys
    | otherwise = Unhandled
    where xs = words s

onlyCoords :: [String] -> [Coord]
onlyCoords xs = case partitionEithers $ map parseCoord xs of
    ([], ys) -> ys
    _ -> error "only coordinates expected"

coordsAndFeed :: [String] -> ([Coord], Feed)
coordsAndFeed xs = case partitionEithers $ map parseCoord xs of
    ([], ys) -> (ys, Nothing)
    ((f:[]), ys) -> (ys, parseFeed f)
    _ -> error "coordinates and feed expected"

parseCoord :: String -> Either String Coord
parseCoord ('X' : xs) = Right $ X $ read xs
parseCoord ('Y' : xs) = Right $ Y $ read xs
parseCoord ('Z' : xs) = Right $ Z $ read xs
parseCoord s = Left s

parseFeed :: String -> Maybe Double
parseFeed ('F' : xs) = Just $ read xs
parseFeed _ = Nothing

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
advance s (Fast xs)
    = let s' = applyCoords xs s
       in (s', Fast $ getCoords s')
advance s (Move xs f)
    = let s' = applyFeed f $ applyCoords xs s
       in (s', Move (getCoords s') $ getFeed s')
advance s c = (s, c)

applyCoords :: [Coord] -> State -> State
applyCoords xs s = foldl g s xs
    where g s (X x') = s { x = Just x' }
          g s (Y y') = s { y = Just y' }
          g s (Z z') = s { z = Just z' }

getCoords :: State -> [Coord]
getCoords State{..} = catMaybes [ X <$> x, Y <$> y, Z <$> z ]

applyFeed :: Feed -> State -> State
applyFeed (Just f') s = s { f = Just f' }
applyFeed _ s = s

getFeed :: State -> Feed
getFeed State{..} = f

main :: IO ()
main = do
    s <- readFile "/home/marten/Desktop/samba/CNC/vco-contour-4mm.nc"
    mapM_ print . expandCode . map parseLine $ lines s

