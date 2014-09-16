-- |
-- Module      : Tests.EncodeByteString
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
module EncodeByteString where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Base32

import Test.Tasty
import Test.Tasty.QuickCheck

import ArbitraryByteString

import Debug.Trace

------------------------------------------------------------------------------
--                             Property checkers                            --
------------------------------------------------------------------------------

assertEq :: (Eq a, Show a) => a -> a -> Bool
assertEq x y
  | x == y    = True
  | otherwise = error $ "error: (" ++ (show x) ++ ") == (" ++ (show y) ++ ")"

prop_encode_bytestring :: ByteString -> Bool
prop_encode_bytestring s
  =  assertEq e1 e2
  && assertEq d2 s
  where
    e1 = encodeOrError s
    d1 = decodeOrError e1
    e2 = encodeOrError d1
    d2 = decodeOrError e2

prop_guess_encoded_length :: ByteString -> Bool
prop_guess_encoded_length s = (guess - B.length e) < 8
  where
    e = encodeOrError s
    guess = guessEncodedLength $ B.length s

encodeOrError d = either error id $ encode d
decodeOrError d = either error id $ decode d
