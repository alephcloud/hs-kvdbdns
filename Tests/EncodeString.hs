-- |
-- Module      : Tests.EncodeString
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
module EncodeString where

import Data.String.Base32

------------------------------------------------------------------------------
--                             Property checkers                            --
------------------------------------------------------------------------------

prop_encode_string :: String -> Bool
prop_encode_string s = d1 == d2 && d1 == s
  where
    e1 = encode s
    d1 = decode e1
    d2 = decode $ encode d1
