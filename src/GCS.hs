{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
import qualified Data.ByteString.Char8 as B
import System.IO
import System.Directory
import System.Process (system)
import System.Console.Haskeline
import System.Console.CmdArgs
import System.Hardware.Serialport
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad
import Data.List (stripPrefix)
import Data.IORef

data Options = Options
    { port :: FilePath
    , dir  :: FilePath
    } deriving (Show, Data, Typeable)

instance Default Options where def = Options "COM3" "."

type Line = (Int, B.ByteString)
type GCRef = ([Line], [Line])

main :: IO ()
main = do
    Options{..} <- cmdArgs def
    withSerial port defaultSerialSettings { commSpeed = CS115200 } $ \port -> do
        liftIO $ setCurrentDirectory dir
        liftIO $ threadDelay 100000
        getSerial port >>= putStr . B.unpack
        processInput port =<< liftIO (newIORef ([], []))

processInput :: SerialPort -> IORef GCRef -> IO ()
processInput port gcref = runInputT defaultSettings loop
    where loop :: InputT IO ()
          loop = getInputLine "> " >>= \x -> case x of
              Nothing -> quit
              Just cmd | Just x <- stripPrefix ":" cmd -> if x == "q" then quit else do
                  e <- liftIO $ try $ localCommand port gcref x
                  either (\(SomeException x) -> liftIO $ print x) return e
                  loop
              Just cmd -> do
                  liftIO $ send port $ B.pack $ cmd ++ "\n"
                  res <- liftIO $ getSerial port
                  outputStr $ B.unpack res
                  loop
          quit = do
              outputStr "quitting..."
              liftIO $ threadDelay 1000000
              return ()

localCommand :: SerialPort -> IORef GCRef -> String -> IO ()
localCommand port gcref cmd = case cmd of
    _ | Just fp <- stripPrefix "l " cmd -> load gcref fp
    _ | Just dir <- stripPrefix "cd " cmd -> setCurrentDirectory dir
    "pwd"       -> getCurrentDirectory >>= putStrLn
    "s"         -> void $ singleStep port gcref
    "r"         -> void $ iterateWhile id $ singleStep port gcref
    "x"         -> reset port
    "rew"       -> modifyIORef' gcref $ \(h, t) -> (t ++ h, [])
    ('!':str)   -> void $ system str
    _           -> error "unrecognized gcs command"

getOneLine :: SerialPort -> IO B.ByteString
getOneLine port = loop where
    loop = do
        x <- recv port 1
        case B.unpack x of
           "\n" -> return x
           _ -> B.append x <$> loop

getSerial :: SerialPort -> IO B.ByteString
getSerial port = loop where
    loop = do
        x <- recv port 256
        if B.null x then return B.empty else B.append x `liftM` loop

bang :: String -> IO ()
bang = void . system

load :: IORef GCRef -> FilePath -> IO ()
load gcref fp = do
    gcode <- B.lines . B.filter (/='\r') <$> B.readFile fp
    mapM_ (putStrLn . B.unpack) $ take 10 gcode
    putStrLn $ show (length gcode) ++ " lines"
    writeIORef gcref (zip [1..] gcode, [])

singleStep :: SerialPort -> IORef GCRef -> IO Bool
singleStep port gcref = readIORef gcref >>= \gc -> case gc of
    ([], _) -> return False
    ((x@(i, s):xs), ys) -> do
        let gcode = B.unpack s
        putStr $ show i ++ "\t" ++ gcode
        hFlush stdout
        case gcode of
            ('T':_) -> do
                putStr "tool change (press enter when done):"
                hFlush stdout
                void getLine
            _ -> do
                send port $ B.snoc s '\n'
                res <- liftIO $ getOneLine port
                putStr $ pad 40 (B.length s) ++ B.unpack res
        writeIORef gcref (xs, ys ++ [x])
        return True

reset :: SerialPort -> IO ()
reset port = do
    liftIO $ send port $ B.pack "\^X"
    res <- liftIO $ getSerial port
    putStr $ B.unpack res

pad :: Int -> Int -> String
pad w l = replicate (max (w - l) 0) ' '
