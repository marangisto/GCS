import qualified Data.ByteString.Char8 as B
import System.Console.Haskeline
import System.Hardware.Serialport
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad

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
              Just ":q" -> return ()
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

