-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Tests
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
--

module Main
    ( main
    ) where
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
