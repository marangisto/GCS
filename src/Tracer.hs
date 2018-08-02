module Tracer (main, State(..), traceBlocks) where

import Parser (GCode(..), parseGCode)
import System.FilePath
import Data.Default
import Data.Maybe

-- G01 G02 G03 G04
data Motion = Rapid | Linear | ArcCW | ArcCCW deriving (Eq, Show)

-- G17 G18 G19
data Plane = XY | ZX | YZ deriving (Eq, Show)

-- G20 G21
data Units = Inches | MilliMeters deriving (Eq, Show)

-- G54 = 1, G55 G56 G57 G58 G59
type Fixture = Int

-- G90 G91
data PosMode = Absolute | Incremental deriving (Eq, Show)

-- G94
data FeedMode = PerMinute deriving (Eq, Show)


data State = State
    { motion    :: Motion
    , plane     :: Plane
    , units     :: Units
    , fixture   :: Fixture
    , posMode   :: PosMode
    , feedMode  :: FeedMode
    , f         :: Maybe Double
    , x         :: Maybe Double
    , y         :: Maybe Double
    , z         :: Maybe Double
    , i         :: Maybe Double
    , j         :: Maybe Double
    , k         :: Maybe Double
    } deriving (Show)

instance Default State where
    def = State Rapid XY Inches 1 Absolute PerMinute def def def def def def def

applyCode :: GCode -> State -> State
applyCode (G 0) s = s { motion = Rapid }
applyCode (G 1) s = s { motion = Linear }
applyCode (G 2) s = s { motion = ArcCW }
applyCode (G 3) s = s { motion = ArcCCW }
applyCode (G 17) s = s { plane = XY }
applyCode (G 18) s = s { plane = ZX }
applyCode (G 19) s = s { plane = YZ }
applyCode (G 20) s = s { units = Inches }
applyCode (G 21) s = s { units = MilliMeters }
applyCode (G n) s | n >= 54 && n <= 59 = s { fixture = n - 53 }
applyCode (G 90) s = s { posMode = Absolute }
applyCode (G 91) s = s { posMode = Incremental }
applyCode (G 94) s = s { feedMode = PerMinute }
applyCode (F v) s = s { f = Just v }
applyCode (X v) s = s { x = Just v }
applyCode (Y v) s = s { y = Just v }
applyCode (Z v) s = s { z = Just v }
applyCode (I v) s = s { i = Just v }
applyCode (J v) s = s { j = Just v }
applyCode (K v) s = s { k = Just v }
applyCode (M 3) s = s -- FIXME
applyCode (M 6) s = s -- FIXME
applyCode (M 8) s = s -- FIXME
applyCode (M 9) s = s -- FIXME
applyCode (M 30) s = s -- FIXME
applyCode (T n) s = s -- FIXME
applyCode (S rpm) s = s -- FIXME
applyCode Percent s = s -- FIXME
applyCode (Comment c) s = s -- FIXME
applyCode c _ = error $ "unhandled GCODE: " ++ show c

applyBlock :: [GCode] -> State -> State
applyBlock gs s = foldl (flip applyCode) s { i = Nothing, j = Nothing, k = Nothing } gs

traceBlocks :: [[GCode]] -> [State]
traceBlocks = scanl (flip applyBlock) def

main :: IO ()
main = do
    let dir = "/home/marten/Desktop/samba/CNC"
        fn = "vco-contour-4mm"
    xs <- (catMaybes . parseGCode . lines) <$> readFile (dir </> fn <.> "nc")
    mapM_ print $ traceBlocks xs

