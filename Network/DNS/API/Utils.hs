-- |
-- Module      : Network.DNS.API.Utils
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE OverloadedStrings #-}
module Network.DNS.API.Utils
    ( validateFQDN
    , appendFQDN
    , splitToNodes
    , popRootNode
    , removeFQDNSuffix
    , guessEncodedLength
    ) where

import Data.Byteable
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (isJust)

import Network.DNS.API.Types
import Network.DNS.API.Error

-- | Append an FQDNEncoded to a valid FQDN
appendFQDN :: FQDNEncoded -- ^ encoded FQDN
           -> FQDN        -- ^ a valid FQDN
           -> Dns FQDN    -- ^ validateFQDN $ FQDNEncoded ++ . ++ FQDN
appendFQDN encoded dom =
    validateFQDN fqdn
  where
    fqdn :: FQDNEncoded
    fqdn = encodeFQDN $ B.concat [ toBytes encoded, ".", toBytes dom ]

-- | remove the given FQDN suffix from the FQDNEncoded
removeFQDNSuffix :: FQDNEncoded
                 -> FQDN
                 -> Dns FQDNEncoded
removeFQDNSuffix encoded dom =
    case makeRemoveSuffix encNodes domNodes of
        Left err -> errorDns err
        Right l  -> return $ nodesToFQDN $ reverse l
  where
    domNodes :: [FQDNEncoded]
    domNodes = reverse $ splitToNodes $ encodeFQDN $ toBytes dom

    encNodes :: [FQDNEncoded]
    encNodes = reverse $ splitToNodes encoded

    makeRemoveSuffix :: [FQDNEncoded] -> [FQDNEncoded] -> Either String [FQDNEncoded]
    makeRemoveSuffix a  [] = Right a
    makeRemoveSuffix [] (_:_)  = Left "suffix larger than data"
    makeRemoveSuffix (x:xs) (y:ys)
        | x == y    = makeRemoveSuffix xs ys
        | otherwise = Left "suffix does not match"

splitToNodes :: FQDNEncoded -> [FQDNEncoded]
splitToNodes fqdn = map encodeFQDN $ B.split '.' $ toBytes fqdn

nodesToFQDN :: [FQDNEncoded] -> FQDNEncoded
nodesToFQDN = encodeFQDN . B.intercalate "." . map toBytes

popRootNode :: FQDNEncoded -> (FQDNEncoded, FQDNEncoded)
popRootNode fqdn =
    let (pre, suf) = B.spanEnd ((/=) '.') bs
    in  (encodeFQDN pre, encodeFQDN suf)
  where
    bs :: ByteString
    bs = toBytes fqdn

-- | Check the FQDNEncoded is a valid FQDN and if it is, it returns the FQDN
--
-- 1. fullLength < 256
-- 2. node's length < 64
-- 3. char are elem of [a-z0-9.-]
validateFQDN :: FQDNEncoded -- ^ the Encoded fqdn
             -> Dns FQDN
validateFQDN req = fullLength req >>= nodeLengths >>= checkAlphabet >>= return . unsafeToFQDN
  where
    fullLength :: FQDNEncoded -> Dns FQDNEncoded
    fullLength fqdn
        | (B.length $ toBytes fqdn) < 256 = return fqdn
        | otherwise = errorDns "Network.DNS.API.Utils: checkEncoding: URL too long"
    
    checkAlphabet :: FQDNEncoded -> Dns FQDNEncoded
    checkAlphabet fqdn
        | B.foldr checkWord8 True $ toBytes fqdn = return fqdn
        | otherwise = errorDns "Network.DNS.API.Utils: checkEncoding: URL contains non-base32-encoded char"

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
        | otherwise = errorDns "Network.DNS.API.Utils: checkEncoding: URL contains too long labels"

    checkNodeW :: Char -> Maybe Int -> Maybe Int
    checkNodeW _ Nothing  = Nothing
    checkNodeW w (Just c)
        | c > 64    = Nothing
        | w == '.'  = Just 0
        | otherwise = Just $ c + 1

-- | Guess what could be the encoded length
--
-- > bs :: ByteString
-- > (length.encoded bs) - (guessEncoded.length bs) < 8
guessEncodedLength :: Int -- ^ the data length (in byte)
                   -> Int -- ^ the maximum length of the encoded data (in byte)
guessEncodedLength 0 = 0
guessEncodedLength l
  | modulo == 0 = 8 * l `div` 5
  | otherwise   = 8 * (l + 5 - modulo) `div` 5
  where
    modulo :: Int
    modulo = l `mod` 5
