-- |
-- Module      :
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Default (def)
import Data.Char (ord)
import Data.ByteString (ByteString)
import qualified Data.ByteString      as S
import Data.Map (Map)
import qualified Data.Map as Map

import Network.DNS.KVDB.Server
import qualified Network.DNS.KVDB.Types as KVDB

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  case args of
    [dom] -> do defaultServer (serverConf dom)
    _ -> putStrLn $ "usage: " ++ name ++ " <Database FQDN>"
  where
    serverConf :: String -> ServerConf
    serverConf dom = def { query  = queryJustEcho (byteStringFromString dom) }

    byteStringFromString :: [Char] -> ByteString
    byteStringFromString s = S.pack $ map (fromIntegral.ord) s

------------------------------------------------------------------------------
--                          KVDB: queries handling                          --
------------------------------------------------------------------------------

type KeyMap = Map ByteString ByteString

exampleOfDB :: KeyMap
exampleOfDB = Map.fromList exampleDB

exampleDB :: [(ByteString, ByteString)]
exampleDB =
  [ ("short", "a simple key")
  , ("linux", "best kernel ever! <3")
  , ("haskell", "Haskell is an advanced purely-functional programming language. An open-source product of more than twenty years of cutting-edge research, it allows rapid development of robust, concise, correct software. With strong support for integration with other languages, built-in concurrency and parallelism, debuggers, profilers, rich libraries and an active community, Haskell makes it easier to produce flexible, maintainable, high-quality software.")
  ]
-- | Default implementation of a query
-- it returns the received key (expecting a Dummy query)
queryJustEcho :: ByteString -> ByteString -> IO (Maybe ByteString)
queryJustEcho dom req = do
  return $ Map.lookup (KVDB.key request) exampleOfDB
  where
    request :: KVDB.Dummy
    request = KVDB.decode encodedKey
    encodedKey :: ByteString
    encodedKey = S.take (S.length req - S.length dom - 1) req
