-- Copyright (c) 2013-2015 PivotCloud, Inc.
--
-- API
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

-- |
-- Module: API
-- Copyright: Copyright (c) 2013-2015 PivotCloud, Inc.
-- License: Apache License, Version 2.0
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--

module API where

import Network.DNS.API.Types
import Network.DNS.API.Packer
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Parse as BP
import qualified Data.ByteString.Pack  as BP
import Data.Monoid ((<>))
import Data.Word (Word8)

data Command = Command
  { command :: ByteString
  , param   :: ByteString
  } deriving (Show, Eq)

instance Packable Command where
    pack (Command c p) = putSizedByteString c <> putByteString p
    unpack = commandParser

commandPacker :: Command -> Int -> BP.Packer ()
commandPacker (Command c p) sizeOfCommand = do
    let l = fromIntegral sizeOfCommand :: Word8
    BP.putStorable l
    BP.putByteString c
    BP.putByteString p

commandParser :: BP.Parser Command
commandParser = do
    s <- fromIntegral <$> BP.anyByte
    c <- BP.take s
    p <- BP.takeAll
    return $ Command { command = c, param = p }

data Return = Return ByteString
    deriving (Show, Eq)

instance Packable Return where
    pack (Return b) = putByteString b
    unpack = Return <$> BP.takeAll
