-- |
-- Module      : Tests.EncodeResponse
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
module EncodeResponse where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)

import Test.Tasty
import Test.Tasty.QuickCheck

import Control.Applicative
import ArbitraryByteString

import Network.DNS.API.Types

instance Arbitrary Response where
  arbitrary =
    let genTxt = arbitrary :: Gen ByteString
        genSignature n = vectorOf n (arbitrary :: Gen Word8) >>= return . B.pack
    in  do
      sizeSignature <- choose (4, 12)
      sig <- genSignature sizeSignature
      txt <- genTxt
      return $ Response txt sig

prop_encode_response :: Response -> Bool
prop_encode_response resp
  =  d1 == d2
  && d2 == resp
  where
    e1 = encodeResponse resp
    d1 = decodeResponse e1
    d2 = decodeResponse $ encodeResponse d1
