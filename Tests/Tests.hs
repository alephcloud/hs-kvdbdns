-- |
-- Module      : Tests.Tests
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import EncodeString
import EncodeByteString

main :: IO ()
main =
  defaultMain
    [ tests_encode
    ]
  where
    tests_encode = testGroup "Encoding"
      [ testProperty "Encode String"        prop_encode_string
      , testProperty "Encode ByteString"    prop_encode_bytestring
      , testProperty "Guess encoded length" prop_guess_encoded_length
      ]
