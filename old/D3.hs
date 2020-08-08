import Graphics.UI.GLUT
 
main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Hello World"
    displayCallback $= display
    reshapeCallback $= Just reshape
    mainLoop

reshape :: ReshapeCallback
reshape size = do
    viewport $= (Position 0 0, size)
    postRedisplay Nothing
 
display :: DisplayCallback
display = do
    clear [ ColorBuffer ]
    renderPrimitive LineStrip $ do
        setColor 0 0 1
        mapM_ mkVertex
            [ (0, 0, 0)
            , (0.5, 0.5, 0)
            , (-0.3, -0.7, 0)
            , (0.3, -0.7, 0)
            ]
        setColor 1 1 0
        mapM_ mkVertex
            [ (0.2, 0.3, 0)
            , (0.5, 0.7, 0)
            ]
    flush

setColor :: GLfloat -> GLfloat -> GLfloat -> IO ()
setColor r g b = color $ Color3 r g b

mkVertex :: (GLfloat, GLfloat, GLfloat) -> IO ()
mkVertex (x, y, z) = vertex $ Vertex3 x y z

