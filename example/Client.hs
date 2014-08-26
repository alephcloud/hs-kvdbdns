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
    [k, d] -> do
      list <- sendQueryDefault $ Dummy (byteStringFromString d) (byteStringFromString k)
      case list of
        Left err -> error $ show err
        Right l  -> print l
    _ -> putStrLn $ "usage: " ++ name ++ " <key> <domain>"

byteStringFromString :: [Char] -> ByteString
byteStringFromString s = B.pack $ map (fromIntegral.ord) s
