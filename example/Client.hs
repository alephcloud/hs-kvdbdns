-- |
-- Module      :
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--

import System.Environment
import Network.DNS.API.Client
import Network.DNS.API.Types

import API

import Data.Char   (ord)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  case args of
    [d, c, p] -> do
      let req = Request
                  { domain = BS.pack d
                  , cmd = Command (BS.pack c) (BS.pack p)
                  , nonce = B.pack [1..12]
                  }
      rs <- makeResolvSeedSafe (Just $ BS.pack d) Nothing Nothing
      rep <- sendQueryDefaultTo rs req :: IO (Either String (Response ByteString))
      case rep of
        Left err -> error  err
        Right re -> do print $ "nonce == signature ? " ++ (show $ (signature re) == (B.pack [1..12]))
                       print re
    _ -> putStrLn $ "usage: " ++ name ++ " <domain> <echo|db> <param>"
