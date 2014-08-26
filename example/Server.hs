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
import Data.Maybe
import Data.Char (ord)
import Data.Monoid (mconcat)
import qualified Data.ByteString.Lazy as SL (toChunks)
import Data.ByteString (ByteString)
import qualified Data.ByteString      as S
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad (forever, void)
import Control.Concurrent (forkIO)
import Control.Applicative ((<$>))

import Network.DNS hiding (lookup)
import Network.DNS.KVDB.Server
import qualified Network.DNS.KVDB.Types as KVDB
import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString (sendAll, sendAllTo, recvFrom)

import System.Environment
import System.Timeout

getDefaultSocket :: IO Socket
getDefaultSocket = do
  addrinfos <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   Nothing (Just "domain")
  addrinfo <- maybe (fail "no addr info") return (listToMaybe addrinfos)
  sock <- socket (addrFamily addrinfo) Datagram defaultProtocol
  bindSocket sock (addrAddress addrinfo)
  return sock

-- | a default server: handle queries for ever
defaultServer :: ServerConf -> Socket -> IO ()
defaultServer conf sock =
  forever $ do
    (bs, addr) <- recvFrom sock 512
    forkIO $ do
       eResp <- handleQuery conf bs
       case eResp of
         Right bs -> void $ timeout (3 * 1000 * 1000) (sendAllTo sock bs addr)
         Left err -> putStrLn err

-- | default proxy
--
-- if a query failed, then this method will forward the query to the
-- real DNS realm
proxy :: ResolvConf
      -> DNSFormat
      -> IO (Either String DNSFormat)
proxy rc req = do
  let worker Resolver{..} = do
        let packet = mconcat . SL.toChunks $ encode req
        sendAll dnsSock packet
        receive dnsSock
  rs <- makeResolvSeed rc
  withResolver rs $ \r -> do
      md <- timeout (3 * 1000 * 1000) (worker r)
      return $ case md of
                  Just d -> check d
                  Nothing -> Left "timeout raised in proxy function"
  where
    ident = identifier . header $ req

    check :: DNSFormat -> Either String DNSFormat
    check rsp = let hdr = header rsp
                in  if identifier hdr == ident
                        then Right rsp
                        else Left "Error in header"

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  case args of
    [dom] -> do sock <- getDefaultSocket
                defaultServer (serverConf dom) sock
    _ -> putStrLn $ "usage: " ++ name ++ " <Database FQDN>"
  where
    serverConf :: String -> ServerConf
    serverConf dom =
      def { inFail = proxy defaultResolvConfiguration
          , query  = queryJustEcho (byteStringFromString dom)
          }
    defaultResolvConfiguration :: ResolvConf
    defaultResolvConfiguration = defaultResolvConf { resolvInfo = RCHostName "8.8.8.8" }

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
