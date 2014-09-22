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
import Data.Hourglass.Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Network.DNS.API.Server
import qualified Network.DNS.API.Types as API
import API

import System.Environment
import Control.Monad
import Control.Concurrent
import Control.Monad.Except
import Data.Functor.Identity

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  case args of
    [dom] -> do sl <- getDefaultConnections (Just "8053") (Seconds 3) Nothing >>= return.catMaybes
                defaultServer (serverConf dom) sl
    _     -> putStrLn $ "usage: " ++ name ++ " <Database FQDN>"
  where
    serverConf :: String -> ServerConf Int ByteString
    serverConf dom = createServerConf (queryDummy (byteStringFromString dom))

    byteStringFromString :: [Char] -> ByteString
    byteStringFromString s = S.pack $ map (fromIntegral.ord) s

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
queryDummy :: ByteString
           -> Connection Int
           -> ByteString
           -> IO (Maybe (API.Response ByteString))
queryDummy dom conn req = do
  let eReq = runIdentity $ runExceptT $ API.decode dom req :: Either String ExampleRequest
  print $ "Connection: " ++ (show $ getSockAddr conn) ++ " opened: " ++ (show $ getCreationDate conn)
  case eReq of
    Left err -> return Nothing
    Right r  -> treatRequest r
  where
    sign :: ByteString -> ByteString -> API.Response ByteString
    sign n t = API.Response { signature = n, response = t }

    treatRequest :: API.ExampleRequest -> IO (Maybe (API.Response ByteString))
    treatRequest r =
      return $ case command $ API.cmd r of
                  "echo" -> Just $ sign (API.nonce r) (param $ API.cmd r)
                  "db"   -> maybe Nothing (\p -> Just $ sign (API.nonce r) p) $ Map.lookup (param $ API.cmd r) exampleOfDB
                  _      -> Nothing
