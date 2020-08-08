module Level (main) where

import qualified Data.GCode as GC
import qualified Data.ByteString as BS

main :: IO ()
main = do
    putStrLn "levelling..."
    gc <- BS.readFile "/home/marten/Desktop/samba/CNC/vco-contour-4mm.nc"
    let Right ast = GC.parseOnlyGCode gc
    mapM_ print ast
