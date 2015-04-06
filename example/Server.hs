-- Copyright (c) 2013-2015 PivotCloud, Inc.
--
-- Main
--
-- Please feel free to contact us at licensing@pivotmail.com with any
-- contributions, additions, or other feedback; we would love to hear from
-- you.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.

-- |
-- Module: Main
-- Copyright: Copyright (c) 2013-2015 PivotCloud, Inc.
-- License: Apache License, Version 2.0
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Default (def)
import Data.Char (ord)
import Data.ByteString (ByteString)
import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as BS
import Data.Hourglass.Types
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Network.DNS.API.Server
import Network.DNS.API.Types as API
import Network.DNS.API.Utils as API
import Network.DNS.API.FQDN as API
import Network.DNS.API.Error
import Network.DNS.API.Bind
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
        ["--bind",f] -> do
            sl <- getDefaultConnections (Just "8053") (Seconds 3)
            configfile <- either error id <$> parseBindFile f
            -- Force the resolution of the Chunks in order to get the error message now
            --
            -- If an error occured during parsing the file, we want to know what was this
            -- error before starting the server
            let !(!list', !bindings) = either error id $ execDns
                    $ insertDNSBindings ExampleBinding
                    $ insertDNSBindings DefaultBinding
                    $ return (configfile, emptyDNSBindings)
            defaultServer (createServerConf bindings) sl
        ["--help"] ->
            putStrLn $ intercalate "\n"
                [ printBindingHelp "  " DefaultBinding
                , ""
                , printBindingHelp "  " ExampleBinding
                ]
        _ -> putStrLn $ "usage: " ++ name ++ " --bind <filepath>"

data ExampleBinding = ExampleBinding
instance Binding ExampleBinding where
    getName _ = ["example"]
    getHelp _ =
        [ "This is an example of how you can write and use your own DNS Binding."
        , ""
        , "This binding provides 2 command:"
        , "* echo to repeat in the TXT response the received query"
        , "* db   query one of the value associated to one of the keys:"
        , "       " ++ intercalate " " (map (show . fst) exampleDB)
        , ""
        , "In this example, all the DNS Types are disable but TXT:"
        , "* A     disable"
        , "* AAAA  disable"
        , "* TXT   <Domain Name>"
        , "* NS    disable"
        , "* CNAME disable"
        , "* DNAME disable"
        , "* PTR   disable"
        , "* MX    disable"
        , "* SRV   disable"
        , "* SOA   disable"
        ]

    getA     = notSupportedBinding
    getAAAA  = notSupportedBinding
    getCNAME = notSupportedBinding
    getDNAME = notSupportedBinding
    getPTR   = notSupportedBinding
    getNS    = notSupportedBinding
    getMX    = notSupportedBinding
    getSRV   = notSupportedBinding
    getSOA   = notSupportedBinding
    getTXT   _ _ = return $ BindingFunction queryDummy

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
queryDummy :: Connection
           -> ValidFQDN
           -> DnsIO [ByteString]
queryDummy conn req = do
    let eReq = execDns $ decodeFQDNEncoded req :: Either String Command
    liftIO $ putStrLn $ "Connection: " ++ (show $ getSockAddr conn) ++ " opened: " ++ (show $ getCreationDate conn) ++ " request: " ++ show req
    pureDns $ case eReq of
        Left err -> errorDns err
        Right r  -> do
            l <- treatRequest r >>= packData
            return [l]
  where
    treatRequest :: Command -> Dns Return
    treatRequest r =
        case command r of
            "echo" -> return $ Return $ param  r
            "db"   -> maybe (errorDns "key not found") (return . Return) $ Map.lookup (param r) exampleOfDB
            _      -> errorDns "Command not supported"
