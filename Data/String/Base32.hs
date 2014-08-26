-- |
-- Module      : Data.String.Base32
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
-- Encode and decode a String ([Char]) into base32.
-- > Base32 = [0..9] ++ ['a'..'v']
module Data.String.Base32
    ( encode
    , decode
    ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char
import Data.Word (Word8)

encode :: String -> String
encode []     = []
encode (c1:c2:c3:c4:c5:xs)
  = toChar   (o1 `shiftR` 3)           -- The first 5 bits of c1
  : toChar (((o1 `shiftL` 2) .&. 0x1C) -- The last 3 bits of c1
         .|. (o2 `shiftR` 6))          -- The first 2 bits of c2
  : toChar  ((o2 `shiftR` 1) .&. 0x1F) -- The next 5 bits of c2
  : toChar (((o2 `shiftL` 4) .&. 0x10) -- The last bit of c2
         .|. (o3 `shiftR` 4))          -- The first 4 bits of c3
  : toChar (((o3 `shiftL` 1) .&. 0x1E) -- The last 4 bits of c3
         .|. (o4 `shiftR` 7))          -- The first bit of c4
  : toChar  ((o4 `shiftR` 2) .&. 0x1F) -- The next 5bits of c4
  : toChar (((o4 `shiftL` 3) .&. 0x18) -- The last 2 bits of c4
         .|. (o5 `shiftR` 5))          -- The first 3 bits of c5
  : toChar   (o5 .&. 0x1F)             -- The last 5 bits of c5
  : encode xs
  where
    o1 = ord c1
    o2 = ord c2
    o3 = ord c3
    o4 = ord c4
    o5 = ord c5
encode [c1,c2,c3,c4]
  = toChar   (o1 `shiftR` 3)           -- The first 5 bits of c1
  : toChar (((o1 `shiftL` 2) .&. 0x1C) -- The last 3 bits of c1
         .|. (o2 `shiftR` 6))          -- The first 2 bits of c2
  : toChar  ((o2 `shiftR` 1) .&. 0x1F) -- The next 5 bits of c2
  : toChar (((o2 `shiftL` 4) .&. 0x10) -- The last bit of c2
         .|. (o3 `shiftR` 4))          -- The first 4 bits of c3
  : toChar (((o3 `shiftL` 1) .&. 0x1E) -- The last 4 bits of c3
         .|. (o4 `shiftR` 7))          -- The first bit of c4
  : toChar  ((o4 `shiftR` 2) .&. 0x1F) -- The next 5bits of c4
  : toChar  ((o4 `shiftL` 3) .&. 0x18) -- The last 2 bits of c4
  : ['z']
  where
    o1 = ord c1
    o2 = ord c2
    o3 = ord c3
    o4 = ord c4
encode [c1,c2,c3]
  = toChar   (o1 `shiftR` 3)           -- The first 5 bits of c1
  : toChar (((o1 `shiftL` 2) .&. 0x1C) -- The last 3 bits of c1
         .|. (o2 `shiftR` 6))          -- The first 2 bits of c2
  : toChar  ((o2 `shiftR` 1) .&. 0x1F) -- The next 5 bits of c2
  : toChar (((o2 `shiftL` 4) .&. 0x10) -- The last bit of c2
         .|. (o3 `shiftR` 4))          -- The first 4 bits of c3
  : toChar  ((o3 `shiftL` 1) .&. 0x1E) -- The last 4 bits of c3
  : ['z', 'z', 'z']
  where
    o1 = ord c1
    o2 = ord c2
    o3 = ord c3
encode [c1,c2]
  = toChar   (o1 `shiftR` 3)           -- The first 5 bits of c1
  : toChar (((o1 `shiftL` 2) .&. 0x1C) -- The last 3 bits of c1
         .|. (o2 `shiftR` 6))          -- The first 2 bits of c2
  : toChar  ((o2 `shiftR` 1) .&. 0x1F) -- The next 5 bits of c2
  : toChar  ((o2 `shiftL` 4) .&. 0x10) -- The last bit of c2
  : ['z', 'z', 'z', 'z']
  where
    o1 = ord c1
    o2 = ord c2
encode [c1]
  = toChar  (o1 `shiftR` 3)           -- The first 5 bits of c1
  : toChar ((o1 `shiftL` 2) .&. 0x1C) -- The last 3 bits of c1
  : ['z', 'z', 'z', 'z', 'z', 'z']
  where
    o1 = ord c1

decode :: String -> String
decode [] = []
decode ( c1: c2: c3: c4: c5: c6: c7: c8: cs)
  | c3 == 'z' && c4 == 'z' && c5 == 'z' && c6 == 'z' && c7 == 'z' && c8 == 'z'
    = chr ((o1 `shiftL` 3) .&. 0xF8
       .|. (o2 `shiftR` 2) .&. 0x07)
    : []
  | c5 == 'z' && c6 == 'z' && c7 == 'z' && c8 == 'z'
    = chr ((o1 `shiftL` 3)
       .|. (o2 `shiftR` 2))
    : chr ((o2 `shiftL` 6) .&. 0xc0
       .|. (o3 `shiftL` 1) .&. 0x3e
       .|. (o4 `shiftR` 4) .&. 0x01)
    : []
  | c6 == 'z' && c7 == 'z' && c8 == 'z'
    = chr ((o1 `shiftL` 3)
       .|. (o2 `shiftR` 2))
    : chr ((o2 `shiftL` 6) .&. 0xc0
       .|. (o3 `shiftL` 1) .&. 0x3e
       .|. (o4 `shiftR` 4) .&. 0x01)
    : chr ((o4 `shiftL` 4) .&. 0xF0
       .|. (o5 `shiftR` 1) .&. 0x0F)
    : []
  | c8 == 'z'
    = chr ((o1 `shiftL` 3)
       .|. (o2 `shiftR` 2))
    : chr ((o2 `shiftL` 6) .&. 0xc0
       .|. (o3 `shiftL` 1) .&. 0x3E
       .|. (o4 `shiftR` 4) .&. 0x01)
    : chr ((o4 `shiftL` 4) .&. 0xF0
       .|. (o5 `shiftR` 1) .&. 0x0F)
    : chr ((o5 `shiftL` 7) .&. 0x80
       .|. (o6 `shiftL` 2) .&. 0x7C
       .|. (o7 `shiftR` 3) .&. 0x03)
    : []
  | otherwise
    = chr ((o1 `shiftL` 3)
       .|. (o2 `shiftR` 2))
    : chr ((o2 `shiftL` 6) .&. 0xc0
       .|. (o3 `shiftL` 1) .&. 0x3E
       .|. (o4 `shiftR` 4) .&. 0x01)
    : chr ((o4 `shiftL` 4) .&. 0xF0
       .|. (o5 `shiftR` 1) .&. 0x0F)
    : chr ((o5 `shiftL` 7) .&. 0x80
       .|. (o6 `shiftL` 2) .&. 0x7C
       .|. (o7 `shiftR` 3) .&. 0x03)
    : chr ((o7 `shiftL` 5) .&. 0xE0
       .|. (o8           ) .&. 0x1F)
    : decode cs
  where
    o1 = fromChar c1
    o2 = fromChar c2
    o3 = fromChar c3
    o4 = fromChar c4
    o5 = fromChar c5
    o6 = fromChar c6
    o7 = fromChar c7
    o8 = fromChar c8
decodeString s = error $ "decodeString: bad input: not a base32String: " ++ s

fromChar :: Char -> Int
fromChar c
  | isDigit c = (ord c) - (ord '0')
  | c `elem` ['a'..'v'] = (ord c) - (ord 'a') + 10
  | otherwise = error $ "fromChar: not a valide Char: " ++ [c]

toChar :: Int -> Char
toChar n
  | n < 10 = chr $ ord '0' + n
  | n < 32 = chr $ ord 'a' + n - 10
  | otherwise = error $ "toChar: bad input: can't convert " ++ (show n)
