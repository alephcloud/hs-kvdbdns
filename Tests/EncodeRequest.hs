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

import Test.Tasty
import Test.Tasty.QuickCheck

import Network.DNS.API.Types
import Control.Applicative

import ArbitraryByteString

import Network.DNS.API.Utils

import Control.Monad.Except
import System.IO.Unsafe
import Data.Functor.Identity

data TestRequest = TestRequest (Request ByteString) ByteString
  deriving (Show, Eq)

instance Arbitrary TestRequest where
  arbitrary =
    let genDom     n = vectorOf n (choose (97, 122))       >>= return . B.pack
        genCommand n = vectorOf n (arbitrary :: Gen Word8) >>= return . B.pack
        genNonce   n = vectorOf n (arbitrary :: Gen Word8) >>= return . B.pack
    in  do
      sizeDom   <- choose (2, 6)
      sizeCmd   <- choose (1, 110)
      sizeNonce <- choose (4, 12)
      dom <- genDom sizeDom
      req <- Request dom
              <$> genCommand sizeCmd
              <*> genNonce sizeNonce
      return $ TestRequest req dom

prop_encode_request :: TestRequest -> Bool
prop_encode_request (TestRequest req dom) =
  let e1 = encodeOrError req
      d1 = decodeOrError dom e1
      e2 = encodeOrError d1
      d2 = decodeOrError dom e2
      encoding = either error (\_ -> True) $ runIdentity $ runExceptT $ checkEncoding e1
  in  d1 == d2 && d2 == req && encoding

encodeOrError d = either error id $ runIdentity $ runExceptT $ encode d
decodeOrError dom d = either error id $ runIdentity $ runExceptT $ decode dom d
