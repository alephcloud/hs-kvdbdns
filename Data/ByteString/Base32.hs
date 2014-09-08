{-# LANGUAGE OverloadedStrings #-}
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

import Control.Monad
import Control.Applicative

data Base32Reader = Base32Reader
  { bits   :: Word8
  , nbRead :: Int
  , result :: [Word8]
  } deriving (Show, Eq)

------------------------------------------------------------------------------
--                           Encode a ByteString                            --
------------------------------------------------------------------------------

encode :: (Monad m, Applicative m)
       => ByteString
       -> m ByteString
encode bs = B.pack.reverse.result <$> (eFlushBuffer =<< B.foldl encoder state bs)
  where
    state :: (Monad m, Applicative m) => m Base32Reader
    state = return $ Base32Reader { bits = 0x00, nbRead = 0, result = [] }

encoder :: (Monad m, Applicative m)
        => m Base32Reader
        -> Word8
        -> m Base32Reader
encoder mst w =
  mst >>= \st ->
  case nbRead st of
    0 -> eUpdateBuffer w 1 5 st >>= eFlushBuffer >>= eUpdateBuffer w 6 8
    1 -> eUpdateBuffer w 1 4 st >>= eFlushBuffer >>= eUpdateBuffer w 5 8
    2 -> eUpdateBuffer w 1 3 st >>= eFlushBuffer >>= eUpdateBuffer w 4 8 >>= eFlushBuffer
    3 -> eUpdateBuffer w 1 2 st >>= eFlushBuffer >>= eUpdateBuffer w 3 7 >>= eFlushBuffer >>= eUpdateBuffer w 8 8
    4 -> eUpdateBuffer w 1 1 st >>= eFlushBuffer >>= eUpdateBuffer w 2 6 >>= eFlushBuffer >>= eUpdateBuffer w 7 8
    _ -> fail $ "encoder: got a read size of: " ++ (show $ nbRead st)

eFlushBuffer :: (Monad m, Applicative m)
             => Base32Reader -> m Base32Reader
eFlushBuffer st =  (\c -> Base32Reader { bits = 0, nbRead = 0, result = c:(result st) }) <$> (toWord8 $ fromIntegral $ bits st)

eUpdateBuffer :: (Monad m, Applicative m)
              => Word8
              -> Int
              -> Int
              -> Base32Reader
              -> m Base32Reader
eUpdateBuffer w from to st =
  return $ st { bits = newBits, nbRead = newNbRead }
  where
    newBits :: Word8
    newBits = (bits st) .|. (((w `shiftR` shifterR) .&. mask) `shiftL` shifterL)
    newNbRead :: Int
    newNbRead = (nbRead st) + length

    shifterR :: Int
    shifterR = 8 - to
    shifterL :: Int
    shifterL = 5 - length - (nbRead st)

    length :: Int
    length = to - from + 1
    mask :: Word8
    mask = getMask length

------------------------------------------------------------------------------
--                           Decode a ByteString                            --
------------------------------------------------------------------------------

decode :: (Monad m, Applicative m)
       => ByteString
       -> m ByteString
decode bs = B.pack.reverse.result <$> B.foldl decoder state bs
  where
    state :: (Monad m, Applicative m) => m Base32Reader
    state = return $ Base32Reader { bits = 0x00, nbRead = 0, result = [] }

decoder :: (Monad m, Applicative m)
        => m Base32Reader
        -> Word8
        -> m Base32Reader
decoder mst c =
  mst >>= \st -> fromWord8 c >>= \w ->
  case nbRead st of
    0 -> dUpdateBuffer w 4 8 st
    1 -> dUpdateBuffer w 4 8 st
    2 -> dUpdateBuffer w 4 8 st
    3 -> dUpdateBuffer w 4 8 st >>= dFlushBuffer
    4 -> dUpdateBuffer w 4 7 st >>= dFlushBuffer >>= dUpdateBuffer w 8 8
    5 -> dUpdateBuffer w 4 6 st >>= dFlushBuffer >>= dUpdateBuffer w 7 8
    6 -> dUpdateBuffer w 4 5 st >>= dFlushBuffer >>= dUpdateBuffer w 6 8
    7 -> dUpdateBuffer w 4 4 st >>= dFlushBuffer >>= dUpdateBuffer w 5 8
    _ -> fail $ "decoder: got to readsize of: " ++ (show $ nbRead st) -- this should not happen

dFlushBuffer :: (Monad m, Applicative m)
             => Base32Reader -> m Base32Reader
dFlushBuffer st =
    (return $ st { bits = 0, nbRead = 0, result = (bits st):(result st) })

dUpdateBuffer :: (Monad m, Applicative m)
              => Word8
              -> Int
              -> Int
              -> Base32Reader
              -> m Base32Reader
dUpdateBuffer w from to st =
  return $ st { bits = newBits, nbRead = newNbRead }
  where
    newBits :: Word8
    newBits = (bits st) .|. (((w `shiftR` shifterR) .&. mask) `shiftL` shifterL)
    newNbRead :: Int
    newNbRead = (nbRead st) + length

    shifterR :: Int
    shifterR = if l < 0 then 0-l else 0
      where l = 3 - (nbRead st)

    shifterL :: Int
    shifterL = 8 - length - (nbRead st)

    length :: Int
    length = to - from + 1
    mask :: Word8
    mask = (getMask length)

------------------------------------------------------------------------------
--                                Helpers                                   --
------------------------------------------------------------------------------

getMask :: Int -> Word8
getMask n =
  case n of
    0 -> 0x00 -- 0000 0000
    1 -> 0x01 -- 0000 0001
    2 -> 0x03 -- 0000 0011
    3 -> 0x07 -- 0000 0111
    4 -> 0x0F -- 0000 1111
    5 -> 0x1F -- 0001 1111
    6 -> 0x3F -- 0011 1111
    7 -> 0x7F -- 0111 1111
    _ -> 0xFF -- 1111 1111

fromWord8 :: (Monad m, Applicative m) => Word8 -> m Word8
fromWord8 w =
  case B.index reverseAlphabet $ fromIntegral w of
    c | c < 32 -> return c
    _          -> fail $ "fromWord8: bad input: can't convert " ++ (show w)

toWord8 :: (Monad m, Applicative m) => Int -> m Word8
toWord8 n
  | n < 32 = return $ B.index alphabet n
  | otherwise = fail $ "toWord8: bad input: can't convert " ++ (show n)

alphabet :: ByteString
alphabet = "0123456789abcdefghijklmnopqrstuv"

reverseAlphabet :: ByteString
reverseAlphabet =
  "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
  \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
  \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
  \\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\xff\xff\xff\xff\xff\xff\
  \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
  \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
  \\xff\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\
  \\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\xff\xff\xff\xff\xff\xff\
  \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
  \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
  \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
  \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
  \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
  \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
  \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
  \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"

-- | Guess what could be the encoded length
--
-- > bs :: ByteString
-- > (length.encoded bs) - (guessEncoded.length bs) < 8
guessEncodedLength :: Int -- ^ the data length (in byte)
                   -> Int -- ^ the maximum length of the encoded data (in byte)
guessEncodedLength 0 = 0
guessEncodedLength l
  | modulo == 0 = 8 * l `div` 5
  | otherwise   = 8 * (l + 5 - modulo) `div` 5
  where
    modulo :: Int
    modulo = l `mod` 5
