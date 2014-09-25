-- |
-- Module      : Tests.Tests
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
import Test.Tasty
import Test.Tasty.QuickCheck

import EncodeByteString
import EncodeRequest
import EncodeResponse

main :: IO ()
main =
  defaultMain tests

tests = testGroup "API over DNS"
    [ tests_encode
    , tests_dns_request
    ]
  where
    tests_encode = testGroup "Encoding"
      [ testProperty "Encode ByteString"    prop_encode_bytestring
      , testProperty "Guess encoded length" prop_guess_encoded_length
      ]
    tests_dns_request = testGroup "DNS Request"
      [ testProperty "Encode Request"       prop_encode_request
      , testProperty "Encode Response"      prop_encode_response
      ]
