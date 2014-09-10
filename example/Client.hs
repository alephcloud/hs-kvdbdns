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

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  case args of
    [d, c, p] -> do
      let req = Request
                  { domain = byteStringFromString d
                  , cmd = Command (byteStringFromString c) (byteStringFromString p)
                  , nonce = B.pack [1..12]
                  }
      rep <- sendQueryDefault req :: IO (Either String (Response ByteString))
      case rep of
        Left err -> error  err
        Right re -> do print $ "nonce == signature ? " ++ (show $ (signature re) == (B.pack [1..12]))
                       print re
    _ -> putStrLn $ "usage: " ++ name ++ " <domain> <echo|db> <param>"

byteStringFromString :: [Char] -> ByteString
byteStringFromString s = B.pack $ map (fromIntegral.ord) s
