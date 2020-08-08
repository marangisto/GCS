
main :: IO ()
main = do
    let
       -- millDiameter = 25.4 / 8                -- millimeters
        millDiameter = 10                -- millimeters
        flutes = 3
        chipLoad = 0.003 * 25.4  -- for a 1/8 inch mill
        surfaceSpeed = 400 * (25.4 * 12  / 1000) -- meters per minute
        rpm = 1000 * surfaceSpeed / (pi * millDiameter)
        feed = rpm * flutes * chipLoad

    putStrLn $ "mill = " ++ show millDiameter
    putStrLn $ "surface speed = " ++ show surfaceSpeed
    putStrLn $ "rpm = " ++ show rpm
    putStrLn $ "feed = " ++ show feed
