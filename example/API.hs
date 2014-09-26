-- |
-- Module      :
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
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

instance Encodable Command where
    encode (Command c p) = putSizedByteString c <> putByteString p
    decode = commandParser

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
