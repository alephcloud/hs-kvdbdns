-- |
-- Module      : Data.ByteString.Base32
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
module Data.ByteString.Base32
    ( encode
    , decode
    , guessEncodedLength
    ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)

encode :: ByteString -> ByteString
encode bs =
  B.pack $ encodeList originList
  where
    originList :: [Word8]
    originList = B.unpack bs

decode :: ByteString -> ByteString
decode bs =
  B.pack $ decodeList originList
  where
    originList :: [Word8]
    originList = B.unpack bs

encodeList :: [Word8] -> [Word8]
encodeList []     = []
encodeList (c1:c2:c3:c4:c5:xs)
  = toWord8   (o1 `shiftR` 3)           -- The first 5 bits of c1
  : toWord8 (((o1 `shiftL` 2) .&. 0x1C) -- The last 3 bits of c1
         .|. (o2 `shiftR` 6))          -- The first 2 bits of c2
  : toWord8  ((o2 `shiftR` 1) .&. 0x1F) -- The next 5 bits of c2
  : toWord8 (((o2 `shiftL` 4) .&. 0x10) -- The last bit of c2
         .|. (o3 `shiftR` 4))          -- The first 4 bits of c3
  : toWord8 (((o3 `shiftL` 1) .&. 0x1E) -- The last 4 bits of c3
         .|. (o4 `shiftR` 7))          -- The first bit of c4
  : toWord8  ((o4 `shiftR` 2) .&. 0x1F) -- The next 5bits of c4
  : toWord8 (((o4 `shiftL` 3) .&. 0x18) -- The last 2 bits of c4
         .|. (o5 `shiftR` 5))          -- The first 3 bits of c5
  : toWord8   (o5 .&. 0x1F)             -- The last 5 bits of c5
  : encodeList xs
  where
    o1 = fromIntegral c1
    o2 = fromIntegral c2
    o3 = fromIntegral c3
    o4 = fromIntegral c4
    o5 = fromIntegral c5
encodeList [c1,c2,c3,c4]
  = toWord8   (o1 `shiftR` 3)           -- The first 5 bits of c1
  : toWord8 (((o1 `shiftL` 2) .&. 0x1C) -- The last 3 bits of c1
         .|. (o2 `shiftR` 6))          -- The first 2 bits of c2
  : toWord8  ((o2 `shiftR` 1) .&. 0x1F) -- The next 5 bits of c2
  : toWord8 (((o2 `shiftL` 4) .&. 0x10) -- The last bit of c2
         .|. (o3 `shiftR` 4))          -- The first 4 bits of c3
  : toWord8 (((o3 `shiftL` 1) .&. 0x1E) -- The last 4 bits of c3
         .|. (o4 `shiftR` 7))          -- The first bit of c4
  : toWord8  ((o4 `shiftR` 2) .&. 0x1F) -- The next 5bits of c4
  : toWord8  ((o4 `shiftL` 3) .&. 0x18) -- The last 2 bits of c4
  : [122]
  where
    o1 = fromIntegral c1
    o2 = fromIntegral c2
    o3 = fromIntegral c3
    o4 = fromIntegral c4
encodeList [c1,c2,c3]
  = toWord8   (o1 `shiftR` 3)           -- The first 5 bits of c1
  : toWord8 (((o1 `shiftL` 2) .&. 0x1C) -- The last 3 bits of c1
         .|. (o2 `shiftR` 6))          -- The first 2 bits of c2
  : toWord8  ((o2 `shiftR` 1) .&. 0x1F) -- The next 5 bits of c2
  : toWord8 (((o2 `shiftL` 4) .&. 0x10) -- The last bit of c2
         .|. (o3 `shiftR` 4))          -- The first 4 bits of c3
  : toWord8  ((o3 `shiftL` 1) .&. 0x1E) -- The last 4 bits of c3
  : [122, 122, 122]
  where
    o1 = fromIntegral c1
    o2 = fromIntegral c2
    o3 = fromIntegral c3
encodeList [c1,c2]
  = toWord8   (o1 `shiftR` 3)           -- The first 5 bits of c1
  : toWord8 (((o1 `shiftL` 2) .&. 0x1C) -- The last 3 bits of c1
         .|. (o2 `shiftR` 6))          -- The first 2 bits of c2
  : toWord8  ((o2 `shiftR` 1) .&. 0x1F) -- The next 5 bits of c2
  : toWord8  ((o2 `shiftL` 4) .&. 0x10) -- The last bit of c2
  : [122, 122, 122, 122]
  where
    o1 = fromIntegral c1
    o2 = fromIntegral c2
encodeList [c1]
  = toWord8  (o1 `shiftR` 3)           -- The first 5 bits of c1
  : toWord8 ((o1 `shiftL` 2) .&. 0x1C) -- The last 3 bits of c1
  : [122, 122, 122, 122, 122, 122]
  where
    o1 = fromIntegral c1

decodeList :: [Word8] -> [Word8]
decodeList [] = []
decodeList ( c1: c2: c3: c4: c5: c6: c7: c8: cs)
  | c3 == 122 && c4 == 122 && c5 == 122 && c6 == 122 && c7 == 122 && c8 == 122
    = fromIntegral ((o1 `shiftL` 3) .&. 0xF8
               .|. (o2 `shiftR` 2) .&. 0x07)
    : []
  | c5 == 122 && c6 == 122 && c7 == 122 && c8 == 122
    = fromIntegral ((o1 `shiftL` 3)
               .|. (o2 `shiftR` 2))
    : fromIntegral ((o2 `shiftL` 6) .&. 0xc0
               .|. (o3 `shiftL` 1) .&. 0x3e
               .|. (o4 `shiftR` 4) .&. 0x01)
    : []
  | c6 == 122 && c7 == 122 && c8 == 122
    = fromIntegral ((o1 `shiftL` 3)
               .|. (o2 `shiftR` 2))
    : fromIntegral ((o2 `shiftL` 6) .&. 0xc0
               .|. (o3 `shiftL` 1) .&. 0x3e
               .|. (o4 `shiftR` 4) .&. 0x01)
    : fromIntegral ((o4 `shiftL` 4) .&. 0xF0
               .|. (o5 `shiftR` 1) .&. 0x0F)
    : []
  | c8 == 122
    = fromIntegral ((o1 `shiftL` 3)
               .|. (o2 `shiftR` 2))
    : fromIntegral ((o2 `shiftL` 6) .&. 0xc0
               .|. (o3 `shiftL` 1) .&. 0x3E
               .|. (o4 `shiftR` 4) .&. 0x01)
    : fromIntegral ((o4 `shiftL` 4) .&. 0xF0
               .|. (o5 `shiftR` 1) .&. 0x0F)
    : fromIntegral ((o5 `shiftL` 7) .&. 0x80
               .|. (o6 `shiftL` 2) .&. 0x7C
               .|. (o7 `shiftR` 3) .&. 0x03)
    : []
  | otherwise
    = fromIntegral ((o1 `shiftL` 3)
               .|. (o2 `shiftR` 2))
    : fromIntegral ((o2 `shiftL` 6) .&. 0xc0
               .|. (o3 `shiftL` 1) .&. 0x3E
               .|. (o4 `shiftR` 4) .&. 0x01)
    : fromIntegral ((o4 `shiftL` 4) .&. 0xF0
               .|. (o5 `shiftR` 1) .&. 0x0F)
    : fromIntegral ((o5 `shiftL` 7) .&. 0x80
               .|. (o6 `shiftL` 2) .&. 0x7C
               .|. (o7 `shiftR` 3) .&. 0x03)
    : fromIntegral ((o7 `shiftL` 5) .&. 0xE0
               .|. (o8           ) .&. 0x1F)
    : decodeList cs
  where
    o1 = fromWord8 c1
    o2 = fromWord8 c2
    o3 = fromWord8 c3
    o4 = fromWord8 c4
    o5 = fromWord8 c5
    o6 = fromWord8 c6
    o7 = fromWord8 c7
    o8 = fromWord8 c8
decodeList s = error $ "decodeList: bad input: not a base32BysteString: " ++ (show s)

fromWord8 :: Word8 -> Int
fromWord8 w =
  case mIndex of
    Nothing    -> error $ "fromWord8: bad input: can't convert " ++ (show w)
    Just index -> index
  where
    mIndex :: Maybe Int
    mIndex = B.elemIndex w alphabet

toWord8 :: Int -> Word8
toWord8 n
  | n < 32 = B.index alphabet n
  | otherwise = error $ "toWord8: bad input: can't convert " ++ (show n)

-- Get the encoded length
guessEncodedLength :: Int -> Int
guessEncodedLength 0 = 0
guessEncodedLength l
  | modulo == 0 = 8 * l `div` 5
  | otherwise   = 8 * (l + 5 - modulo) `div` 5
  where
    modulo :: Int
    modulo = l `mod` 5

alphabet :: ByteString
alphabet = B.pack $ [48..57] ++ [97..119]
