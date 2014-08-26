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

import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Base32

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

instance Arbitrary ByteString where
  arbitrary =
    let genWord8 = arbitrary :: Gen Word8
    in  listOf genWord8 >>= return . B.pack

------------------------------------------------------------------------------
--                             Property checkers                            --
------------------------------------------------------------------------------

prop_encode_bytestring :: ByteString -> Bool
prop_encode_bytestring s = d1 == d2 && d1 == s
  where
    e1 = encode s
    d1 = decode e1
    d2 = decode $ encode d1

prop_guess_encoded_length :: ByteString -> Bool
prop_guess_encoded_length s = guess == B.length e
  where
    e = encode s
    guess = guessEncodedLength $ B.length s
