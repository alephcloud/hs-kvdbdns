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

import EncodeRequest
import EncodeResponse

main :: IO ()
main =
  defaultMain tests

tests = testGroup "properties"
    [ tests_dns_request
    ]
  where
    tests_dns_request = testGroup "encode/decode"
      [ testProperty "Request"       prop_encode_request
      , testProperty "Response"      prop_encode_response
      ]
