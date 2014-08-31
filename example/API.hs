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
  unpack bs = pure $ Command { command = c, param = p }
    where
      cmdSize :: Int
      cmdSize = fromIntegral $ B.head bs

      c :: ByteString
      c = B.take cmdSize $ B.drop 1 bs

      p :: ByteString
      p = B.drop (cmdSize + 1) bs

type ExampleRequest = Request Command
