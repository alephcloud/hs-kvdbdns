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

import API
import Control.Monad.Except
import Control.Monad.Identity

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
      let req = Request
                  { cmd = Command (BS.pack c) (BS.pack p)
                  , nonce = uniqueNonce
                  }
      let domBs = BS.pack d
      let d' = runIdentity $ runExceptT $ validateFQDN $ encodeFQDN domBs
      let dom = either (\err -> error $ "the given domain address is not a valid FQDN: " ++ err)
                       (id) d'
      rs <- makeResolvSeedSafe (Just domBs) (Just $ fromIntegral 8053) Nothing Nothing
      rep <- runExceptT $ sendQueryDefaultTo rs req dom :: IO (Either String (Response Return))
      case rep of
        Left err -> error $ "exmaple.Client: " ++ err
        Right re -> do print $ "nonce == signature ? " ++ (show $ (signature re) == uniqueNonce)
                       print re
    _ -> putStrLn $ "usage: " ++ name ++ " <domain> <echo|db> <param>"
