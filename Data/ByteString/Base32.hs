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

import Control.Applicative hiding ((<|>))

infixr 1 <:>
(<:>) :: Applicative m => m a -> m [a] -> m [a]
(<:>) a b = liftA2 (:) a b

-- m a -> a -> ma
infixl 1 <<$>
(<<$>) :: Applicative m => m Word8 -> Int -> m Word8
(<<$>) a b = (shiftL) <$> a <*> pure b

infixl 1 <$>>
(<$>>) :: Applicative m => m Word8 -> Int -> m Word8
(<$>>) a b = (shiftR) <$> a <*> pure b

infixl 2 <&>
(<&>) :: Applicative m => m Word8 -> Word8 -> m Word8
(<&>) a b = (.&.) <$> a <*> pure b

infixl 1 <|>
(<|>) :: Applicative m => m Word8 -> m Word8 -> m Word8
(<|>) a b = (.|.) <$> a <*> b

encode :: (Monad m, Applicative m)
       => ByteString
       -> m ByteString
encode bs = B.pack <$> encodeList originList
  where
    originList :: [Word8]
    originList = B.unpack bs

decode :: (Monad m, Applicative m) => ByteString -> m ByteString
decode bs = B.pack <$> decodeList originList
  where
    originList :: [Word8]
    originList = B.unpack bs

encodeList :: (Applicative m, Monad m) => [Word8] -> m [Word8]
encodeList []     = pure []
encodeList (c1:c2:c3:c4:c5:xs)
  = toWord8   (o1 `shiftR` 3)           -- The first 5 bits of c1
  <:>  toWord8 (((o1 `shiftL` 2) .&. 0x1C) -- The last 3 bits of c1
    .|. (o2 `shiftR` 6))          -- The first 2 bits of c2
  <:>  toWord8  ((o2 `shiftR` 1) .&. 0x1F) -- The next 5 bits of c2
  <:>  toWord8 (((o2 `shiftL` 4) .&. 0x10) -- The last bit of c2
    .|. (o3 `shiftR` 4))          -- The first 4 bits of c3
  <:>  toWord8 (((o3 `shiftL` 1) .&. 0x1E) -- The last 4 bits of c3
    .|. (o4 `shiftR` 7))          -- The first bit of c4
  <:>  toWord8  ((o4 `shiftR` 2) .&. 0x1F) -- The next 5bits of c4
  <:>  toWord8 (((o4 `shiftL` 3) .&. 0x18) -- The last 2 bits of c4
    .|. (o5 `shiftR` 5))          -- The first 3 bits of c5
  <:>  toWord8   (o5 .&. 0x1F)             -- The last 5 bits of c5
  <:>  encodeList xs
  where
    o1 = fromIntegral c1
    o2 = fromIntegral c2
    o3 = fromIntegral c3
    o4 = fromIntegral c4
    o5 = fromIntegral c5
encodeList [c1,c2,c3,c4]
  = toWord8   (o1 `shiftR` 3)           -- The first 5 bits of c1
  <:> toWord8 (((o1 `shiftL` 2) .&. 0x1C) -- The last 3 bits of c1
         .|. (o2 `shiftR` 6))          -- The first 2 bits of c2
  <:> toWord8  ((o2 `shiftR` 1) .&. 0x1F) -- The next 5 bits of c2
  <:> toWord8 (((o2 `shiftL` 4) .&. 0x10) -- The last bit of c2
         .|. (o3 `shiftR` 4))          -- The first 4 bits of c3
  <:> toWord8 (((o3 `shiftL` 1) .&. 0x1E) -- The last 4 bits of c3
         .|. (o4 `shiftR` 7))          -- The first bit of c4
  <:> toWord8  ((o4 `shiftR` 2) .&. 0x1F) -- The next 5bits of c4
  <:> toWord8  ((o4 `shiftL` 3) .&. 0x18) -- The last 2 bits of c4
  <:> pure [122]
  where
    o1 = fromIntegral c1
    o2 = fromIntegral c2
    o3 = fromIntegral c3
    o4 = fromIntegral c4
encodeList [c1,c2,c3]
  = toWord8   (o1 `shiftR` 3)           -- The first 5 bits of c1
  <:> toWord8 (((o1 `shiftL` 2) .&. 0x1C) -- The last 3 bits of c1
         .|. (o2 `shiftR` 6))          -- The first 2 bits of c2
  <:> toWord8  ((o2 `shiftR` 1) .&. 0x1F) -- The next 5 bits of c2
  <:> toWord8 (((o2 `shiftL` 4) .&. 0x10) -- The last bit of c2
         .|. (o3 `shiftR` 4))          -- The first 4 bits of c3
  <:> toWord8  ((o3 `shiftL` 1) .&. 0x1E) -- The last 4 bits of c3
  <:> pure [122, 122, 122]
  where
    o1 = fromIntegral c1
    o2 = fromIntegral c2
    o3 = fromIntegral c3
encodeList [c1,c2]
  = toWord8   (o1 `shiftR` 3)           -- The first 5 bits of c1
  <:> toWord8 (((o1 `shiftL` 2) .&. 0x1C) -- The last 3 bits of c1
         .|. (o2 `shiftR` 6))          -- The first 2 bits of c2
  <:> toWord8  ((o2 `shiftR` 1) .&. 0x1F) -- The next 5 bits of c2
  <:> toWord8  ((o2 `shiftL` 4) .&. 0x10) -- The last bit of c2
  <:> pure [122, 122, 122, 122]
  where
    o1 = fromIntegral c1
    o2 = fromIntegral c2
encodeList [c1]
  =   (toWord8  (o1 `shiftR` 3))           -- The first 5 bits of c1
  <:> (toWord8 ((o1 `shiftL` 2) .&. 0x1C)) -- The last 3 bits of c1
  <:> (pure [122, 122, 122, 122, 122, 122])
  where
    o1 = fromIntegral c1



decodeList :: (Monad m, Applicative m) => [Word8] -> m [Word8]
decodeList [] = pure []
decodeList ( c1: c2: c3: c4: c5: c6: c7: c8: cs)
  | c3 == 122 && c4 == 122 && c5 == 122 && c6 == 122 && c7 == 122 && c8 == 122
    =    ((o1 <<$> 3) <&> 0xF8
      <|> (o2 <$>> 2) <&> 0x07)
    <:> pure []
  | c5 == 122 && c6 == 122 && c7 == 122 && c8 == 122
    =    ((o1 <<$> 3)
      <|> (o2 <$>> 2))
    <:>    ((o2 <<$> 6) <&> 0xc0
      <|> (o3 <<$> 1) <&> 0x3e
      <|> (o4 <$>> 4) <&> 0x01)
    <:> pure []
  | c6 == 122 && c7 == 122 && c8 == 122
    =    ((o1 <<$> 3)
      <|> (o2 <$>> 2))
    <:>    ((o2 <<$> 6) <&> 0xc0
      <|> (o3 <<$> 1) <&> 0x3e
      <|> (o4 <$>> 4) <&> 0x01)
    <:>    ((o4 <<$> 4) <&> 0xF0
      <|> (o5 <$>> 1) <&> 0x0F)
    <:> pure []
 | c8 == 122
    =    ((o1 <<$> 3)
      <|> (o2 <$>> 2))
    <:>    ((o2 <<$> 6) <&> 0xc0
      <|> (o3 <<$> 1) <&> 0x3E
      <|> (o4 <$>> 4) <&> 0x01)
    <:>    ((o4 <<$> 4) <&> 0xF0
      <|> (o5 <$>> 1) <&> 0x0F)
    <:>    ((o5 <<$> 7) <&> 0x80
      <|> (o6 <<$> 2) <&> 0x7C
      <|> (o7 <$>> 3) <&> 0x03)
    <:> pure []
 | otherwise
    =    ((o1 <<$> 3)
      <|> (o2 <$>> 2))
    <:>    ((o2 <<$> 6) <&> 0xc0
      <|> (o3 <<$> 1) <&> 0x3E
      <|> (o4 <$>> 4) <&> 0x01)
    <:>    ((o4 <<$> 4) <&> 0xF0
      <|> (o5 <$>> 1) <&> 0x0F)
    <:>    ((o5 <<$> 7) <&> 0x80
      <|> (o6 <<$> 2) <&> 0x7C
      <|> (o7 <$>> 3) <&> 0x03)
    <:>    ((o7 <<$> 5) <&> 0xE0
      <|> (o8           ) <&> 0x1F)
    <:> decodeList cs
  where
    o1 = fromWord8 c1
    o2 = fromWord8 c2
    o3 = fromWord8 c3
    o4 = fromWord8 c4
    o5 = fromWord8 c5
    o6 = fromWord8 c6
    o7 = fromWord8 c7
    o8 = fromWord8 c8
decodeList s = fail $ "decodeList: bad input: not a base32BysteString: " ++ (show s)

fromWord8 :: (Monad m, Applicative m) => Word8 -> m Word8
fromWord8 w =
  case mIndex of
    Just index -> pure $ fromIntegral index
    Nothing    -> fail $ "fromWord8: bad input: can't convert " ++ (show w)
  where
    mIndex :: Maybe Int
    mIndex = B.elemIndex w alphabet

toWord8 :: (Monad m) => Int -> m Word8
toWord8 n
  | n < 32 = return $ B.index alphabet n
  | otherwise = fail $ "toWord8: bad input: can't convert " ++ (show n)

alphabet :: ByteString
alphabet = B.pack $ [48..57] ++ [97..119]

-- Get the encoded length
guessEncodedLength :: Int -> Int
guessEncodedLength 0 = 0
guessEncodedLength l
  | modulo == 0 = 8 * l `div` 5
  | otherwise   = 8 * (l + 5 - modulo) `div` 5
  where
    modulo :: Int
    modulo = l `mod` 5
