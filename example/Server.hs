-- |
-- Module      :
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--

import Data.Default (def)
import Data.Maybe

import Network.DNS.KVDB.Server
import Network.Socket

import System.Environment

getDefaultSocket :: IO Socket
getDefaultSocket = do
  addrinfos <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   Nothing (Just "domain")
  addrinfo <- maybe (fail "no addr info") return (listToMaybe addrinfos)
  sock <- socket (addrFamily addrinfo) Datagram defaultProtocol
  bindSocket sock (addrAddress addrinfo)
  return sock

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  case args of
    [] -> do sock <- getDefaultSocket
             defaultServer def sock
    _ -> putStrLn $ "usage: " ++ name
