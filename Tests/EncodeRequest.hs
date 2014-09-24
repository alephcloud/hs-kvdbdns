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

import Data.Byteable
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Parse as BP
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

data TestCommand = TestCommand ByteString
    deriving (Show, Eq)

instance Packable TestCommand where
    pack (TestCommand bs) = bs
    unpack = TestCommand <$> BP.takeAll

data TestRequest = TestRequest (Request TestCommand) FQDN
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
      dom <- unsafeToFQDN . encodeFQDN <$> genDom sizeDom
      req <- Request
              <$> (TestCommand <$> genCommand sizeCmd)
              <*> genNonce sizeNonce
      return $ TestRequest req dom

assertEq :: (Eq a, Show a) => a -> a -> Bool
assertEq x y
  | x == y    = True
  | otherwise = error $ "assertEq failed:\n(" ++ (show x) ++ ")\n(" ++ (show y) ++ ")"

prop_encode_request :: TestRequest -> Bool
prop_encode_request (TestRequest req dom) =
    let e1 = encodeOrError req
        d1 = decodeOrError e1
        e2 = encodeOrError d1
        d2 = decodeOrError e2
    in  assertEq d1 d2
     && assertEq d2 req
  where
    encodeOrError d = either error id $ execDns $ (encode d >>= \t -> appendFQDN t dom)
    decodeOrError d = either error id $ execDns $ decodeFQDNEncoded $ removeFQDNSuffix (encodeFQDN $ toBytes d) dom
