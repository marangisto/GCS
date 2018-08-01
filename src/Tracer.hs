module Tracer (main, State(..), traceBlocks) where

import Parser (GCode(..), parseGCode)
import System.FilePath
import Data.Default
import Data.Maybe

data State = State
    { x :: Maybe Double
    , y :: Maybe Double
    , z :: Maybe Double
    , f :: Maybe Double
    } deriving (Show)

instance Default State where
    def = State def def def def

applyCode :: GCode -> State -> State
applyCode (X v) s = s { x = Just v }
applyCode (Y v) s = s { y = Just v }
applyCode (Z v) s = s { z = Just v }
applyCode _ s = s

applyBlock :: [GCode] -> State -> State
applyBlock gs s = foldl (flip applyCode) s gs

traceBlocks :: [[GCode]] -> [State]
traceBlocks = scanl (flip applyBlock) def

main :: IO ()
main = do
    let dir = "/home/marten/Desktop/samba/CNC"
        fn = "vco-contour-4mm"
    xs <- (catMaybes . parseGCode . lines) <$> readFile (dir </> fn <.> "nc")
    mapM_ print $ traceBlocks xs

