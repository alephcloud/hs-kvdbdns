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
import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as BS
import Data.Hourglass.Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Network.DNS.API.Server
import Network.DNS.API.Types as API
import Network.DNS.API.Utils as API
import Network.DNS.API.Error
import API

import System.Environment
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Identity

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  case args of
    [d] -> do let d' = execDns $ validateFQDN $ encodeFQDN $ BS.pack d
              let dom = either (\err -> error $ "the given domain address is not a valid FQDN: " ++ err)
                               (id) d'
              sl <- getDefaultConnections (Just "8053") (Seconds 3) Nothing >>= return.catMaybes
              defaultServer (serverConf dom) sl
    _     -> putStrLn $ "usage: " ++ name ++ " <Database FQDN>"
  where
    serverConf :: FQDN -> ServerConf Int
    serverConf dom = createServerConf { queryTXT = queryDummy dom }

------------------------------------------------------------------------------
--                          API: queries handling                          --
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

-- | example of query manager
-- handle two commands:
-- * echo: the param
-- * db  : return the DB
--
-- This actual example just ignore the connection context and information
queryDummy :: API.FQDN
           -> Connection Int
           -> API.FQDNEncoded
           -> DnsIO ByteString
queryDummy dom conn req = do
  let eReq = execDns $ decodeFQDNEncoded =<< removeFQDNSuffix req dom :: Either String Command
  liftIO $ print $ "Connection: " ++ (show $ getSockAddr conn) ++ " opened: " ++ (show $ getCreationDate conn)
  pureDns $ case eReq of
    Left err -> errorDns err
    Right r  -> treatRequest r >>= packData
  where
    treatRequest :: Command -> Dns Return
    treatRequest r =
        case command r of
            "echo" -> return $ Return $ param  r
            "db"   -> maybe (errorDns "key not found") (return . Return) $ Map.lookup (param r) exampleOfDB
            _      -> errorDns "Command not supported"
