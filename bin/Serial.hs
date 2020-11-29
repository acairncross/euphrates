import Data.Word
import System.Environment (getArgs)
import System.Hardware.Serialport
import qualified Data.ByteString as BS

-- Send some bytes over the serial port
main :: IO ()
main = do
  input <- getContents
  let bytes = map read . words $ input :: [Word8]
  withSerial "/dev/ttyUSB0" (defaultSerialSettings { commSpeed = CS115200 }) $ \port ->
    send port (BS.pack bytes) >> return ()
