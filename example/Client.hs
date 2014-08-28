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
import Network.DNS.KVDB.Types
import Network.DNS.KVDB.Client

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
                  , cmd = byteStringFromString c
                  , nonce = B.pack [1..12]
                  , param = byteStringFromString p
                  }
      list <- sendQueryDefault req
      case list of
        Left err -> error $ show err
        Right re -> do print $ "nonce == signature ? " ++ (show $ (signature re) == (B.pack [1..12]))
                       print re
    _ -> putStrLn $ "usage: " ++ name ++ " <domain> <echo|db> <param>"

byteStringFromString :: [Char] -> ByteString
byteStringFromString s = B.pack $ map (fromIntegral.ord) s
