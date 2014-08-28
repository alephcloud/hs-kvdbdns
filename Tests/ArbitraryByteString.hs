-- |
-- Module      :
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
module ArbitraryByteString where

import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary ByteString where
  arbitrary =
    let genWord8 = arbitrary :: Gen Word8
    in  listOf genWord8 >>= return . B.pack
