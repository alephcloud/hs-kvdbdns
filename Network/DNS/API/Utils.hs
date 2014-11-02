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
    ( guessEncodedLength
    ) where

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
