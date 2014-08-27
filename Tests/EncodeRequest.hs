-- |
-- Module      : Tests.EncodeRequest
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module EncodeRequest where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Network.DNS.KVDB.Types
import Control.Applicative

import ArbitraryByteString

data TestRequest = TestRequest Request ByteString
  deriving (Show, Eq)

instance Arbitrary TestRequest where
  arbitrary =
    let genParam = arbitrary :: Gen ByteString
        genCommand n = vectorOf n (arbitrary :: Gen Word8) >>= return . B.pack
        genNonce   n = vectorOf n (arbitrary :: Gen Word8) >>= return . B.pack
    in  do
      dom       <- genParam
      sizeCmd   <- choose (1, 35)
      sizeNonce <- choose (1, 35)
      req <- Request dom
              <$> genCommand sizeCmd
              <*> genNonce sizeNonce
              <*> genParam
      return $ TestRequest req dom

prop_encode_request :: TestRequest -> Bool
prop_encode_request (TestRequest req dom) = d1 == d2 && d1 == req
  where
    e1 = encode req
    d1 = decode dom e1
    d2 = decode dom $ encode d1
