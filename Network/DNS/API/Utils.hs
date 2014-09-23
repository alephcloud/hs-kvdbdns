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
   ( validateFQDN
   ) where

import Control.Monad.Except
import Data.Byteable
import qualified Data.ByteString.Char8 as B
import Data.Maybe (isJust)

import Network.DNS.API.Types

-- | Check that the given bytestring (the Domain) respect some rules:
--
-- 1. fullLength < 256
-- 2. node's length < 64
-- 3. char are elem of [a-z0-9.-]
validateFQDN :: FQDNEncoded
             -> Dns FQDN
validateFQDN invalidefqdn = fullLength invalidefqdn >>= nodeLengths >>= checkAlphabet >>= return . unsafeToFQDN
  where
    fullLength :: FQDNEncoded -> Dns FQDNEncoded
    fullLength fqdn
        | (B.length $ toBytes fqdn) < 256 = return fqdn
        | otherwise = throwError "Network.DNS.API.Utils: checkEncoding: URL too long"
    
    checkAlphabet :: FQDNEncoded -> Dns FQDNEncoded
    checkAlphabet fqdn
        | B.foldr checkWord8 True $ toBytes fqdn = return fqdn
        | otherwise = throwError "Network.DNS.API.Utils: checkEncoding: URL contains non-base32-encoded char"

    checkWord8 :: Char -> Bool -> Bool
    checkWord8 _ False = False
    checkWord8 c True
        | c <= 'z' && c >= 'a' = True
        | c <= '9' && c >= '0' = True
        | c == '.' || c == '-' = True
        | otherwise = False

    nodeLengths :: FQDNEncoded -> Dns FQDNEncoded
    nodeLengths fqdn
        | isJust $ B.foldr checkNodeW (Just 0) $ toBytes fqdn = return fqdn
        | otherwise = throwError "Network.DNS.API.Utils: checkEncoding: URL contains too long labels"

    checkNodeW :: Char -> Maybe Int -> Maybe Int
    checkNodeW _ Nothing  = Nothing
    checkNodeW w (Just c)
        | c > 64    = Nothing
        | w == '.'  = Just 0
        | otherwise = Just $ c + 1
