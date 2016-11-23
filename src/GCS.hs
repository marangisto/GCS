import qualified Data.ByteString.Char8 as B
import System.Directory
import System.Console.Haskeline
import System.Hardware.Serialport
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad
import Data.List (stripPrefix)
import Data.IORef

type GCRef = ([B.ByteString], [B.ByteString])

main :: IO ()
main = withSerial "COM10" defaultSerialSettings { commSpeed = CS115200 } $ \port -> do
    liftIO $ setCurrentDirectory "//GOLEM/marten/Fusion 360 CAM/nc"
    liftIO $ threadDelay 100000
    getSerial port >>= putStr . B.unpack
    processInput port =<< liftIO (newIORef ([], []))

processInput :: SerialPort -> IORef GCRef -> IO ()
processInput port gcref = runInputT defaultSettings loop
    where loop :: InputT IO ()
          loop = getInputLine "> " >>= \x -> case x of
              Nothing -> quit
              Just cmd | Just x <- stripPrefix ":" cmd -> if x == "q" then quit else do
                  e <- liftIO $ try $ case words x of
                      [ "pwd" ]    -> getCurrentDirectory >>= putStrLn
                      ("cd":dir:[])-> setCurrentDirectory dir
                      ("ls":args)  -> ls args
                      ("l":fp:[])  -> load gcref fp
                      ("s":[])     -> void $ singleStep port gcref
                      ("r":[])     -> void $ iterateWhile id $ singleStep port gcref
                      ("rew":[])     -> modifyIORef' gcref $ \(h, t) -> (h ++ t, [])
                      _            -> error "unrecognized gcs command"
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

ls :: [String] -> IO ()
ls args = do
    mdir <- case args of
        []     -> fmap Just $ getCurrentDirectory
        (x:[]) -> return $ Just x
        _      -> return Nothing
    case mdir of
        Just dir -> listDirectory dir >>= mapM_ putStrLn
        _        -> error "invalid argument"

load :: IORef GCRef -> FilePath -> IO ()
load gcref fp = do
    gcode <- B.lines . B.filter (/='\r') <$> B.readFile fp
    mapM_ (putStrLn . B.unpack) $ take 10 gcode
    putStrLn $ show (length gcode) ++ " lines"
    writeIORef gcref (gcode, [])

singleStep :: SerialPort -> IORef GCRef -> IO Bool
singleStep port gcref = readIORef gcref >>= \gc -> case gc of
    ([], _) -> return False
    ((x:xs), ys) -> do
        putStrLn $ B.unpack x
        send port $ B.snoc x '\n'
        res <- liftIO $ getOneLine port
        putStr $ B.unpack res
        writeIORef gcref (xs, ys ++ [x])
        return True

