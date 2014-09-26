-- |
-- Module      :
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import Network.DNS.API.Client
import Network.DNS.API.Types
import Network.DNS.API.Utils
import Network.DNS.API.Error

import API

import Data.Char   (ord)
import Data.Word (Word8)
import Data.Either
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS

uniqueNonce :: ByteString
uniqueNonce = "nonce"

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  case args of
    [d, c, p] -> do
      let req = Command (BS.pack c) (BS.pack p)
      let domBs = BS.pack d
      let dom = either (\err -> error $ "the given domain address is not a valid FQDN: " ++ err)
                       (id) $ execDns $ validateFQDN $ encodeFQDN domBs
      rs <- makeResolvSeedSafe (Just domBs) (Just $ fromIntegral 8053) Nothing Nothing
      rep <- execDnsIO $ sendQueryTo rs req dom :: IO (Either String Return)
      case rep of
        Left err -> error $ "exmaple.Client: " ++ err
        Right re -> print re
    _ -> putStrLn $ "usage: " ++ name ++ " <domain> <echo|db> <param>"
