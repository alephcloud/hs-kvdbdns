-- |
-- Module      : Network.DNS.KVDB.Types
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.DNS.KVDB.Types
  ( Encodable(..)
  , Dummy(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base32 as BSB32

import Data.List (intercalate)

class Encodable a where
  encode :: a -> ByteString
  decode :: ByteString -> a

data Dummy = Dummy
  { domain :: ByteString
  , key    :: ByteString
  } deriving (Show, Eq)

instance Encodable Dummy where
  encode req = B.intercalate (B.pack [0x2E]) [encode k, d]
    where
      d :: ByteString
      d = domain req
      k :: ByteString
      k = key req
  decode bs = Dummy B.empty (decode bs)

instance Encodable ByteString where
  encode = encodeURL
  decode = decodeURL

encodeURL :: ByteString -> ByteString
encodeURL bs
  | guessedLength > 200 = error "bytestring too long"
  | otherwise = B.intercalate (B.pack [0x2E]) $ splitByNode e
  where
    e :: ByteString
    e = BSB32.encode bs
    guessedLength :: Int
    guessedLength = BSB32.guessEncodedLength $ B.length bs

    splitByNode :: ByteString -> [ByteString]
    splitByNode bs
      | B.length bs < 63 = [bs]
      | otherwise = node:(splitByNode xs)
      where
        (node, xs) = B.splitAt 62 bs

decodeURL :: ByteString -> ByteString
decodeURL bs = BSB32.decode fusion
  where
    fusion :: ByteString
    fusion = B.concat $ B.split 0x2E bs
