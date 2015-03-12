-- Copyright (c) 2013-2015 PivotCloud, Inc.
--
-- Network.DNS.API.Utils
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
