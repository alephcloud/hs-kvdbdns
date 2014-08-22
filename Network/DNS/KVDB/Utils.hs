-- |
-- Module      : Network.DNS.KVDB.Utils
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DNS.KVDB.Utils
   ( checkEncoding
   ) where

import Network.DNS.KVDB.Types

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

-- | Check that the given bytestring (the Domain) respect some rules:
--
-- 1. fullLength < 256
-- 2. node's length < 64
checkEncoding :: ByteString
              -> Bool
checkEncoding bs
  =  fullLength
  && nodeLengths
  where
    fullLength :: Bool
    fullLength = B.length bs < 256
    
    nodeLengths :: Bool
    nodeLengths = maybe False (\_ -> True) $ B.foldr checkNodeW (Just 0) bs

    checkNodeW :: Char -> Maybe Int -> Maybe Int
    checkNodeW _ Nothing  = Nothing
    checkNodeW w (Just c)
        | c > 64    = Nothing
        | w == '.'  = Just 0
        | otherwise = Just $ c + 1
