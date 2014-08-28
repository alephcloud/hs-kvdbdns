-- |
-- Module      : Network.DNS.API.Utils
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DNS.API.Utils
   ( checkEncoding
   ) where

import Network.DNS.API.Types

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

-- | Check that the given bytestring (the Domain) respect some rules:
--
-- 1. fullLength < 256
-- 2. node's length < 64
-- 3. char are elem of [a-z0-9.-]
checkEncoding :: ByteString
              -> Bool
checkEncoding bs
  =  fullLength
  && nodeLengths
  && checkAlphabet
  where
    fullLength :: Bool
    fullLength = B.length bs < 256
    
    checkAlphabet :: Bool
    checkAlphabet = B.foldr checkWord8 True bs

    checkWord8 :: Char -> Bool -> Bool
    checkWord8 _ False = False
    checkWord8 c True
        | c <= 'z' && c >= 'a' = True
        | c <= '9' && c >= '0' = True
        | c == '.' || c == '-' = True
        | otherwise = False

    nodeLengths :: Bool
    nodeLengths = maybe False (\_ -> True) $ B.foldr checkNodeW (Just 0) bs

    checkNodeW :: Char -> Maybe Int -> Maybe Int
    checkNodeW _ Nothing  = Nothing
    checkNodeW w (Just c)
        | c > 64    = Nothing
        | w == '.'  = Just 0
        | otherwise = Just $ c + 1
