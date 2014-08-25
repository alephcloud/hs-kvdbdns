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

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  case args of
    [k, d] -> do
      let dom = Dummy d k
      putStrLn $ show $ encode dom
      list <- sendQueryDefault dom
      case list of
        Left err -> error $ show err
        Right l  -> do print l
                       mapM_ print l
    _ -> putStrLn $ "usage: " ++ name ++ " <key> <domain>"
