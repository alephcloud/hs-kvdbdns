-- Copyright (c) 2013-2015 PivotCloud, Inc.
--
-- EncodeRequest
--
-- Please feel free to contact us at licensing@pivotmail.com with any
-- contributions, additions, or other feedback; we would love to hear from
-- you.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module EncodeRequest where

import Data.Byteable
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Parse as BP
import qualified Data.ByteString.Pack  as BP
import Data.Monoid ((<>))
import Data.Word (Word8)

import Test.Tasty
import Test.Tasty.QuickCheck

import Network.DNS.API.Types
import Network.DNS.API.FQDN
import Network.DNS.API.Error
import Network.DNS.API.Packer
import Control.Applicative

import ArbitraryByteString

import Network.DNS.API.Utils

import Control.Monad.Except
import System.IO.Unsafe
import Data.Functor.Identity

data TestCommand = TestCommand Word8 ByteString
    deriving (Show, Eq)

instance Packable TestCommand where
    pack (TestCommand w bs) = putWord8 w <> putByteString bs
    unpack = TestCommand <$> BP.anyByte <*> BP.takeAll

data TestRequest = TestRequest TestCommand ValidFQDN
  deriving (Show, Eq)

instance Arbitrary TestRequest where
  arbitrary =
    let genDom     n = vectorOf n (choose (97, 122))       >>= return . B.pack
        genCommand n = vectorOf n (arbitrary :: Gen Word8) >>= return . B.pack
    in  do
      sizeDom   <- choose (2, 6)
      sizeCmd   <- choose (1, 110)
      dom <- unsafeValidFQDN <$> genDom sizeDom
      req <- TestCommand <$> choose (0, 255)
                         <*> genCommand sizeCmd
      return $ TestRequest req dom

assertEq :: (Eq a, Show a) => a -> a -> Bool
assertEq x y
  | x == y    = True
  | otherwise = error $ "assertEq failed:\n(" ++ (show x) ++ ")\n(" ++ (show y) ++ ")"

prop_encode_request :: TestRequest -> Bool
prop_encode_request (TestRequest req dom) =
    let e1 = encodeOrError req :: ValidFQDN
        d1 = decodeOrError e1  :: TestCommand
        e2 = encodeOrError d1  :: ValidFQDN
        d2 = decodeOrError e2  :: TestCommand
    in  assertEq d1 d2
     && assertEq d2 req
  where
    decodeOrError d = either error id $ execDns $ decodeFQDNEncoded =<< (removeFQDNSuffix d dom :: Dns ByteString)
    encodeOrError d =
        let encoded = either error id $ execDns $ encodeFQDNEncoded d :: UnsafeFQDN
        in  either error id $ execDns $ appendFQDN encoded dom
