-- |
-- Module      : Tests.EncodeResponse
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE FlexibleInstances #-}
module EncodeResponse where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Parse as BP
import qualified Data.ByteString.Pack  as BP
import Data.Monoid ((<>))
import Data.Word (Word8)

import Test.Tasty
import Test.Tasty.QuickCheck

import Control.Applicative
import ArbitraryByteString

import Network.DNS.API.Types
import Network.DNS.API.Error
import Network.DNS.API.Packer

import Control.Monad.Except
import Data.Functor.Identity

data TestResponse = TestResponse ByteString ByteString
    deriving (Show, Eq)

takeSizedString :: BP.Parser ByteString
takeSizedString =
    (fromIntegral <$> BP.anyByte) >>= BP.take

instance Packable TestResponse where
    pack (TestResponse b1 b2) = putSizedByteString b1 <> putByteString b2
    unpack = TestResponse <$> takeSizedString
                          <*> BP.takeAll

instance Arbitrary TestResponse where
  arbitrary = TestResponse <$> arbitrary <*> arbitrary

prop_encode_response :: TestResponse -> Bool
prop_encode_response resp =
    let d1 = encodeDecode resp
        d2 = encodeDecode d1
    in  d1 == d2 && d2 == resp
  where
    encodeDecode d = either (error) id $ execDns $ (packData d >>= unpackData) 
