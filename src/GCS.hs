import qualified Data.ByteString.Char8 as B
import System.Directory
import System.Console.Haskeline
import System.Hardware.Serialport
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad
import Data.List (stripPrefix)

main :: IO ()
main = withSerial "COM3" defaultSerialSettings { commSpeed = CS115200 } $ \s -> do
    liftIO $ threadDelay 100000
    getSerial s >>= putStr . B.unpack
    processInput s

processInput :: SerialPort -> IO ()
processInput s = runInputT defaultSettings loop
    where loop :: InputT IO ()
          loop = getInputLine "> " >>= \x -> case x of
              Nothing -> return ()
              Just cmd | Just x <- stripPrefix ":" cmd -> if x == "q" then return () else do
                  e <- liftIO $ try $ case words x of
                      [ "pwd" ]    -> getCurrentDirectory >>= putStrLn
                      ("cd":dir:[])-> setCurrentDirectory dir
                      ("ls":args)  -> ls args
                      _            -> error "unrecognized gcs command"
                  either (\(SomeException x) -> liftIO $ print x) return e
                  loop
              Just cmd -> do
                  liftIO $ send s $ B.pack $ cmd ++ "\n"
                  res <- liftIO $ getSerial s
                  outputStr $ B.unpack res
                  loop

getSerial :: SerialPort -> IO B.ByteString
getSerial s = loop where
    loop = do
        x <- recv s 256
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

