{-# LANGUAGE RecordWildCards #-}
module Level (main) where

import Data.Maybe
import Data.Either
import Data.Default
import Data.List (sort, nub)
import System.FilePath
import Debug.Trace

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
    | U String
    deriving (Eq, Ord, Show)

parseGC :: String -> GCode
parseGC ('G' : xs) = G $ read xs
parseGC ('M' : xs) = M $ read xs
parseGC ('T' : xs) = T $ read xs
parseGC ('X' : xs) = X $ read xs
parseGC ('Y' : xs) = Y $ read xs
parseGC ('Z' : xs) = Z $ read xs
parseGC ('I' : xs) = I $ read xs
parseGC ('J' : xs) = J $ read xs
parseGC ('K' : xs) = K $ read xs
parseGC ('F' : xs) = F $ read xs
parseGC ('S' : xs) = S $ read xs
parseGC "%" = Percent
parseGC s = U $ filter (/='\r') s

parseBlock :: String -> [GCode]
parseBlock s@('(':_) = [ Comment $ filter (/='\r') s ]
parseBlock s = map parseGC $ words s

usedCodes :: [[GCode]] -> [GCode]
usedCodes = nub . sort . filter p . concat
    where p (G _) = True
          p (M _) = True
          p _ = False

data Action = None | Move deriving (Enum, Show)

-- G0 G1 G2 G3
data Motion = Rapid | Linear | ArcCW | ArcCCW deriving (Enum, Show)

-- G17 G18 G19
data Plane = XY | ZX | YZ deriving (Enum, Show)

-- G20 G21
data Units = Inches | MilliMeters deriving (Enum, Show)

-- G54 = 1, G55 G56 G57 G58 G59
--data Fixture = Fixture Int deriving (Show)

-- G90 G91
data PosMode = Absolute | Incremental deriving (Enum, Show)

-- G94
data FeedMode = PerMinute deriving (Enum, Show)

data State = State
    { action    :: Action
    , motion    :: Motion
    , plane     :: Plane
    , units     :: Units
    , fixture   :: Int
    , posMode   :: PosMode
    , feedMode  :: FeedMode
    , xPos      :: Maybe Double
    , yPos      :: Maybe Double
    , zPos      :: Maybe Double
    , feed      :: Maybe Double
    } deriving (Show)

instance Default State where
    def = State None Rapid XY Inches 1 Absolute PerMinute def def def def

updateState :: GCode -> State -> State
updateState (G 0) s = s { motion = Rapid }
updateState (G 1) s = s { motion = Linear }
updateState (G 2) s = s { motion = ArcCW }
updateState (G 3) s = s { motion = ArcCCW }
updateState (G 17) s = s { plane = XY }
updateState (G 18) s = s { plane = ZX }
updateState (G 19) s = s { plane = YZ }
updateState (G 20) s = s { units = Inches }
updateState (G 21) s = s { units = MilliMeters }
updateState (G n) s | n >= 54 && n <= 59 = s { fixture = n - 53 }
updateState (G 90) s = s { posMode = Absolute }
updateState (G 91) s = s { posMode = Incremental }
updateState (G 94) s = s { feedMode = PerMinute }
updateState (F f) s = s { feed = Just f }
updateState (X l) s@State{..} = s { xPos = applyDistance s l xPos, action = Move }
updateState (Y l) s@State{..} = s { yPos = applyDistance s l yPos, action = Move }
updateState (Z l) s@State{..} = s { zPos = applyDistance s l zPos, action = Move }
updateState (I l) s = s -- FIXME
updateState (J l) s = s -- FIXME
updateState (K l) s = s -- FIXME
updateState (M 3) s = s -- FIXME
updateState (M 6) s = s -- FIXME
updateState (M 8) s = s -- FIXME
updateState (M 9) s = s -- FIXME
updateState (M 30) s = s -- FIXME
updateState (T n) s = s -- FIXME
updateState (S rpm) s = s -- FIXME
updateState Percent s = s -- FIXME
updateState (Comment c) s = s -- FIXME
updateState c _ = error $ "unhandled gcode: " ++ show c

applyDistance :: State -> Double -> Maybe Double -> Maybe Double
applyDistance State{ posMode = Absolute } l _ = Just l
applyDistance State{ posMode = Incremental } l (Just v) = Just (l + v)

interpretBlock :: [GCode] -> State -> State
interpretBlock xs s = foldl (flip updateState) (s { action = None }) xs

interpret :: [[GCode]] -> [State]
interpret = scanl (flip interpretBlock) def

main :: IO ()
main = do
    let dir = "/home/marten/Desktop/samba/CNC"
        fn = "vco-bore-4mm"
    s <- readFile $ dir </> fn <.> "nc"
    let bs = map parseBlock $ lines s
    mapM_ print bs
    mapM_ print $ usedCodes bs
    mapM_ print $ interpret bs

