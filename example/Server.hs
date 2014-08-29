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

import Network.DNS.API.Server
import qualified Network.DNS.API.Types as API

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
    serverConf dom = def { query  = queryDummy (byteStringFromString dom) }

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
-- * echo: return $ Just $ "nonce" ++ ';' ++ "param"
-- * db  : return $ the result of a lookup in the database
--
-- This actual example just ignore who sent it.
queryDummy :: ByteString -> a -> ByteString -> IO (Maybe API.Response)
queryDummy dom _ req =
  return $ case API.cmd request of
              "echo" -> Just $ sign (API.nonce request) (API.param request)
              "db"   -> maybe Nothing (\p -> Just $ sign (API.nonce request) p) $ Map.lookup (API.param request) exampleOfDB
              _      -> Nothing
  where
    request :: API.Request
    request = API.decode dom req

    sign :: ByteString -> ByteString -> API.Response
    sign n t = API.Response { signature = n, response = t }
