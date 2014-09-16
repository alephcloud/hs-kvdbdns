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
import Control.Monad.Except
import Data.Maybe (isJust)

-- | Check that the given bytestring (the Domain) respect some rules:
--
-- 1. fullLength < 256
-- 2. node's length < 64
-- 3. char are elem of [a-z0-9.-]
checkEncoding :: ByteString
              -> Dns ByteString
checkEncoding bs = fullLength bs >>= nodeLengths >>= checkAlphabet
  where
    fullLength :: ByteString -> Dns ByteString
    fullLength b
        | B.length bs < 256 = return b
        | otherwise = throwError "Network.DNS.API.Utils: checkEncoding: URL too long"
    
    checkAlphabet :: ByteString -> Dns ByteString
    checkAlphabet b 
        | B.foldr checkWord8 True bs = return b
        | otherwise = throwError "Network.DNS.API.Utils: checkEncoding: URL contains non-base32-encoded char"

    checkWord8 :: Char -> Bool -> Bool
    checkWord8 _ False = False
    checkWord8 c True
        | c <= 'z' && c >= 'a' = True
        | c <= '9' && c >= '0' = True
        | c == '.' || c == '-' = True
        | otherwise = False

    nodeLengths :: ByteString -> Dns ByteString
    nodeLengths b
        | isJust $ B.foldr checkNodeW (Just 0) b = return b
        | otherwise = throwError "Network.DNS.API.Utils: checkEncoding: URL contains too long labels"

    checkNodeW :: Char -> Maybe Int -> Maybe Int
    checkNodeW _ Nothing  = Nothing
    checkNodeW w (Just c)
        | c > 64    = Nothing
        | w == '.'  = Just 0
        | otherwise = Just $ c + 1
