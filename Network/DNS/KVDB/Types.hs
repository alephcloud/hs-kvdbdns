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

import Data.Char (ord, chr)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.List (intercalate)

class Encodable a where
  encode :: a -> ByteString
  decode :: ByteString -> a

instance Encodable ByteString where
  encode = id
  decode = id

instance Encodable String where
  encode = B.pack.map (fromIntegral.ord)
  decode = map (chr.fromIntegral) . B.unpack

-- | This is an example about what can be done
data Dummy = Dummy
  { domainServer :: String
  , key          :: String
  }

instance Encodable Dummy where
  encode (Dummy d k) = encode $ k ++ "." ++ d
  decode s =
    Dummy
      (intercalate "." d)
      (intercalate "." k)
    where
      nodes  = map (funToString.B.unpack) $ B.split dot s
      size   = length nodes
      (k, d) = splitAt (size - 3) nodes

      dot :: Word8
      dot = fromIntegral $ ord '.'

      funToString :: [Word8] -> [Char]
      funToString = map (chr.fromIntegral)
