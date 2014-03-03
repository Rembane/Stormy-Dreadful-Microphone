{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import qualified Data.Attoparsec.Char8 as A
import qualified Data.Bits as Bi
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Maybe
import Data.Word
import System.Hardware.Serialport 
import Types

charWord8 :: A.Parser Word8
charWord8 = A.anyChar >>= (return . fromIntegral . fromEnum)

signedParser :: A.Parser Int
signedParser = A.signed A.decimal

parseXYZ :: A.Parser GyroValues
parseXYZ = do
    A.string "X: "
    x <- signedParser
    A.string " Y: "
    y <- signedParser
    A.string " Z: "
    z <- signedParser
    A.char '\n'

    return $ GyroValues x y z

parseBenjaminUART :: A.Parser BenjaminUART
parseBenjaminUART = do
    some (A.char '~')
    len     <- fmap (+ ((-2) :: Word8)) charWord8 
    typ     <- charWord8
    action  <- charWord8
    payload <- (A.take (fromIntegral len :: Int)) 
    crcH    <- charWord8
    crcL    <- charWord8
    A.char '~'

    let crc = (Bi.shiftL (fromIntegral crcH :: Word16) 8) + (fromIntegral crcL :: Word16)

    return $ BenjaminUART len typ action payload crc

-- This is for testing purposes
createBenjaminString :: Int -> Int -> String -> B.ByteString
createBenjaminString typ action payload = B.pack $ concat ["~", convert $ length payload, convert typ, convert action, payload, "1", "2", "~"]
    where
        convert c = [toEnum c :: Char]

benjaminString :: B.ByteString
benjaminString = createBenjaminString 0 255 "X: -12 Y: -30 Z: 7\n"

getInput :: SerialPort -> IO B.ByteString
getInput s = recv s 100

cleaningParser :: A.Parser ()
cleaningParser = A.skipWhile (/= '~')

parseChunk :: SerialPort -> B.ByteString -> (B.ByteString -> A.Result BenjaminUART) -> IO BenjaminUART
parseChunk port buf f = do 
    s <- fmap (B.append buf) (getInput port)
    go (f s)
    where
        go (A.Fail _ _ _) = parseChunk port (fromMaybe "" $ A.maybeResult $ A.parse (cleaningParser >> A.skipWhile (== '~') >> A.takeByteString) buf) f
        go (A.Partial f') = parseChunk port "" f'
        go (A.Done _ r)   = return r

portName = "/dev/ttyUSB0"

main :: IO ()
main = do
    port <- openSerial portName defaultSerialSettings { commSpeed = CS115200 } 
    forever $ do
        b2 <- (parseChunk port "" (A.parse $ cleaningParser >> parseBenjaminUART)) 
        -- print b2
        -- print $ payload b2
        let result = A.parse parseXYZ $ payload b2 
        case result of 
            (A.Done _ (GyroValues x y z)) -> putStrLn $ intercalate "\t" $ map show [x,y,z]
            _                             -> putStrLn "Error"
    closeSerial port
