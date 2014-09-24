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
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Parse as BP

data Command = Command
  { command :: ByteString
  , param   :: ByteString
  } deriving (Show, Eq)

instance Packable Command where
    pack (Command c p) =
        B.concat
            [ B.pack [fromIntegral $ B.length c]
            , c
            , p
            ]
    unpack = do
        s <- fromIntegral <$> BP.anyByte
        c <- BP.take s
        p <- BP.takeAll
        return $ Command { command = c, param = p }

data Return = Return ByteString
    deriving (Show, Eq)

instance Packable Return where
    pack (Return b) = b
    unpack = Return <$> BP.takeAll

type ExampleRequest = Request Command
