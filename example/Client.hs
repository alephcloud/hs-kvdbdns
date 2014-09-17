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

import API
import Control.Monad.Except

import Data.Char   (ord)
import Data.Word (Word8)
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
                  { domain = BS.pack d
                  , cmd = Command (BS.pack c) (BS.pack p)
                  , nonce = uniqueNonce
                  }
      rs <- makeResolvSeedSafe (Just $ BS.pack d) (Just $ fromIntegral 8053) Nothing Nothing
      rep <- runExceptT $ sendQueryDefaultTo rs req :: IO (Either String (Response ByteString))
      case rep of
        Left err -> error $ "exmaple.Client: " ++ err
        Right re -> do print $ "nonce == signature ? " ++ (show $ (signature re) == uniqueNonce)
                       print re
    _ -> putStrLn $ "usage: " ++ name ++ " <domain> <echo|db> <param>"
