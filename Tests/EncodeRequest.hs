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

import Network.DNS.KVDB.Utils

data TestRequest = TestRequest Request ByteString
  deriving (Show, Eq)

instance Arbitrary TestRequest where
  arbitrary =
    let genParam = arbitrary :: Gen ByteString
        genDom     n = vectorOf n (choose (97, 122))       >>= return . B.pack
        genCommand n = vectorOf n (arbitrary :: Gen Word8) >>= return . B.pack
        genNonce   n = vectorOf n (arbitrary :: Gen Word8) >>= return . B.pack
    in  do
      sizeDom   <- choose (2, 6)
      sizeCmd   <- choose (1, 35)
      sizeNonce <- choose (1, 35)
      dom <- genDom sizeDom
      req <- Request dom
              <$> genCommand sizeCmd
              <*> genNonce sizeNonce
              <*> genParam
      return $ TestRequest req dom

prop_encode_request :: TestRequest -> Bool
prop_encode_request (TestRequest req dom)
  =  d1 == d2
  && d2 == req
  && checkEncoding e1
  where
    e1 = encode req
    d1 = decode dom e1
    d2 = decode dom $ encode d1
