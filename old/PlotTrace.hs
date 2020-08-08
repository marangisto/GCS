module PlotTrace (main) where

import Parser (GCode(..), parseGCode)
import Tracer (State(..), traceBlocks)
import Graphics.UI.GLUT
import System.FilePath
import Data.Maybe

type Point = (Double, Double, Double)

extractPoint :: State -> Point
extractPoint s = (fromMaybe 0 $ x s, fromMaybe 0 $ y s, fromMaybe 0 $ z s)

main :: IO ()
main = do
    let dir = "/home/marten/Desktop/samba/CNC"
        fn = "vco-bore-4mm"
    xs <- (catMaybes . parseGCode . lines) <$> readFile (dir </> fn <.> "nc")
    let ps = map extractPoint $ traceBlocks xs
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Hello World"
    displayCallback $= (display $ normalizePoints ps)
    reshapeCallback $= Just reshape
    mainLoop

normalizePoints :: [Point] -> [Point]
normalizePoints ps = map f ps
    where f (x, y, z) = (k * (x - ax) - 0.5, k * (y - ay) - 0.5, k * (z - az) - 0.5)
          k = 1 / maximum [ abs $ bx - ax, abs $ by - ay, abs $ bz - az ]
          (ax, bx) = range $ map (\(x, _, _) -> x) ps
          (ay, by) = range $ map (\(_, y, _) -> y) ps
          (az, bz) = range $ map (\(_, _, z) -> z) ps
          range ks = (minimum ks, maximum ks)

reshape :: ReshapeCallback
reshape size = do
    viewport $= (Position 0 0, size)
    postRedisplay Nothing
 
display :: [Point] -> DisplayCallback
display ps = do
    clear [ ColorBuffer ]
    renderPrimitive LineStrip $ do
        setColor 1 1 0
        mapM_ (mkVertex . fromPoint) ps
    flush

setColor :: GLfloat -> GLfloat -> GLfloat -> IO ()
setColor r g b = color $ Color3 r g b

mkVertex :: (GLfloat, GLfloat, GLfloat) -> IO ()
mkVertex (x, y, z) = vertex $ Vertex3 x y z

fromPoint :: (Double, Double, Double) -> (GLfloat, GLfloat, GLfloat)
fromPoint (x, y, z) = (realToFrac x, realToFrac y, realToFrac z)

