-- | The cartridge related functions
module Cartridge (
    Zone (..),
    bankCount,
    hasRightSize,
    header,
    mapper,
    zone,
) where

import Control.Lens (Ixed (ix), (^?))
import Data.Bits (Bits (..))
import qualified Data.ByteString as B

data Zone
    = JapanSMS
    | ExportSMS
    | JapanGG
    | ExportGG
    | InternationalGG
    deriving (Show)

data Mapper = RomOnly | Sega
    deriving (Show)

header :: ByteString -> Maybe Int
header bs = find headerIsAt [0x7FF0, 0x1FF0, 0x3FF0]
  where
    headerIsAt l = "TMR SEGA" `B.isPrefixOf` B.drop l bs

zone :: ByteString -> Maybe Zone
zone bs = toZone =<< zoneIndex (header bs)
  where
    zoneIndex Nothing = pure 3
    zoneIndex (Just h) = do
        b <- bs ^? ix (h + 0x0F)
        pure $ b `shiftR` 4 .&. 0x0F

    toZone 3 = Just JapanSMS
    toZone 4 = Just ExportSMS
    toZone 5 = Just JapanGG
    toZone 6 = Just ExportGG
    toZone 7 = Just InternationalGG
    toZone _ = Nothing

hasRightSize :: ByteString -> Bool
hasRightSize bs = rem (B.length bs) 1024 == 0

-- | Find the closest power of two that verifies: x <= n^2
pow2Ceil :: Int -> Int
pow2Ceil = execState $ do
    modify ((-1) +)
    modify (\x -> x `shiftR` 1 .|. x)
    modify (\x -> x `shiftR` 2 .|. x)
    modify (\x -> x `shiftR` 4 .|. x)
    modify (\x -> x `shiftR` 8 .|. x)
    modify (+ 1)

bankCount :: Int -> Int
bankCount l = max 1 $ pow2Ceil $ l `div` 0x4000

mapper :: Int -> Mapper
mapper l
    | l <= 0xC000 = RomOnly
    | otherwise = Sega
