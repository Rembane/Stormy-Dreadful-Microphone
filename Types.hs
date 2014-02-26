{-# LANGUAGE OverloadedStrings #-}
module Types (BenjaminUART(..), GyroValues(..), payload, serializeGyroValues) where

import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Word
import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random

-- Length, type, action, payload, crc
data BenjaminUART = BenjaminUART Word8 Word8 Word8 B.ByteString Word16
    deriving (Show)

payload :: BenjaminUART -> B.ByteString
payload (BenjaminUART _ _ _ p _) = p

-- TODO: Implement CRC
instance Arbitrary BenjaminUART where
    arbitrary = do 
        typ     <- arbitrary
        action  <- arbitrary
        payload <- fmap serializeGyroValues gyroGen
        crc     <- arbitrary

        let len = fromIntegral (B.length payload) :: Word8

        return $ BenjaminUART len typ action payload crc

        where
            gyroGen :: Gen GyroValues
            gyroGen = arbitrary

data GyroValues = GyroValues Int Int Int
    deriving (Show)

instance Arbitrary GyroValues where
    arbitrary = GyroValues <$> arbitrary <*> arbitrary <*> arbitrary 

serializeGyroValues :: GyroValues -> B.ByteString
serializeGyroValues (GyroValues x y z) = B.concat $ concat $ transpose [map B.pack ["X: ", " Y: ", " Z: "], map (B.pack . show) [x, y, z]]

{-
arbitraryGyroValues :: Int -> [GyroValues]
arbitraryGyroValues seed = unGen arbitrary (mkStdGen seed) 7 
     
arbitraryBenjaminUART :: Int -> [BenjaminUART]
arbitraryBenjaminUART seed = unGen arbitrary (mkStdGen seed) 7 
-}
