-- |
-- Module      : Network.DNS.KVDB.Server
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Network.DNS.KVDB.Server
  ( handleQuery
  , defaultServer
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent  (forkIO)
import Control.Monad       (void, forever)

import Data.ByteString (ByteString)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as SL (toChunks, fromChunks)
import Data.Default
import Data.Maybe  (listToMaybe)
import Data.Monoid (mconcat)
import Data.Char   (ord)
import Data.Word   (Word8)

import Network.DNS hiding (lookup)
import qualified Network.DNS.KVDB.Types as KVDB

import Network.Socket            (Socket, SockAddr)
import Network.Socket.ByteString (sendAll, sendAllTo, recvFrom)

import System.Timeout

------------------------------------------------------------------------------
--                         Server Configuration                             --
------------------------------------------------------------------------------

-- | Server configuration
data ServerConf = ServerConf
  { bufSize :: Int -- ^ the maximum size accpeted per request
  , timeOut :: Int -- ^ timeout for sending/receiving
  , query   :: ByteString -> IO (Maybe ByteString) -- ^ the method to perform a request
  , inFail  :: DNSFormat  -> IO (Maybe DNSFormat)  -- ^ the method to use to handle query failure
  }

-- Default
instance Default ServerConf where
    def = ServerConf {
        bufSize = 512
      , timeOut = defaultTimeOut
      , query   = queryJustEcho
      , inFail  = proxy defaultResolvConf defaultTimeOut
    }
      where
        defaultTimeOut :: Int
        defaultTimeOut = 3 * 1000 * 1000
        defaultResolvConfiguration :: ResolvConf
        defaultResolvConfiguration = defaultResolvConf { resolvInfo = RCHostName "8.8.8.8" }

-- | Default implementation of a query
-- it returns the received key (expecting a Dummy query)
queryJustEcho :: ByteString -> IO (Maybe ByteString)
queryJustEcho req = return $ Just $ S.pack $ funFromString $ KVDB.key request
  where
    request :: KVDB.Dummy
    request = KVDB.decode req
    funFromString :: [Char] -> [Word8]
    funFromString = map (fromIntegral.ord)

-- | default proxy
--
-- if a query failed, then this method will forward the query to the 
-- real DNS realm
proxy :: ResolvConf
      -> Int
      -> DNSFormat  -- ^ the request
      -> IO (Maybe DNSFormat)
proxy rc t req = do
  let worker Resolver{..} = do
        let packet = mconcat . SL.toChunks $ encode req
        sendAll dnsSock packet
        receive dnsSock
  rs <- makeResolvSeed rc
  withResolver rs $ \r ->
      (>>= check) <$> timeout t (worker r)
  where
    ident = identifier . header $ req

    check :: DNSFormat -> Maybe DNSFormat
    check rsp = let hdr = header rsp
                in  if identifier hdr == ident
                        then Just rsp
                        else Nothing

------------------------------------------------------------------------------
--                         The main function                                --
------------------------------------------------------------------------------

-- Handle a request:
-- try the query function given in the ServerConf
-- if it fails, then call the given proxy
handleRequest :: ServerConf -> DNSFormat -> IO (Maybe DNSFormat)
handleRequest conf req =
  case listToMaybe . filterTXT . question $ req of
    Just q -> do
        mres <- query conf $ qname q
        case mres of
           Just txt -> return $ Just $ responseTXT ident q txt
           Nothing  -> inFail conf req
    Nothing -> inFail conf req
  where
    filterTXT = filter ((==TXT) . qtype)
    ident = identifier . header $ req

    responseTXT :: Int -> Question -> ByteString -> DNSFormat
    responseTXT ident q txt =
      let hd = header defaultResponse
          dom = qname q
          an = ResourceRecord dom TXT 1 (S.length txt) (RD_TXT txt)
      in  defaultResponse
            { header = hd { identifier = ident, qdCount = 1, anCount = 1 }
            , question = [q]
            , answer = [an]
            }

    -- imported from dns:Network/DNS/Internal.hs
    defaultQuery :: DNSFormat
    defaultQuery = DNSFormat {
        header = DNSHeader {
           identifier = 0
         , flags = DNSFlags {
               qOrR         = QR_Query
             , opcode       = OP_STD
             , authAnswer   = False
             , trunCation   = False
             , recDesired   = True
             , recAvailable = False
             , rcode        = NoErr
             }
         , qdCount = 0
         , anCount = 0
         , nsCount = 0
         , arCount = 0
         }
      , question   = []
      , answer     = []
      , authority  = []
      , additional = []
      }

    -- imported from dns:Network/DNS/Internal.hs
    defaultResponse :: DNSFormat
    defaultResponse =
      let hd = header defaultQuery
          flg = flags hd
      in  defaultQuery {
            header = hd {
              flags = flg {
                  qOrR = QR_Response
                , authAnswer = True
                , recAvailable = True
                }
        }
      }

-- | handle a DNS query
handleQuery :: ServerConf
            -> Socket     -- ^ Should be a Datagram socket
            -> SockAddr   -- ^ the sender addr
            -> ByteString -- ^ the query
            -> IO ()
handleQuery conf sock addr bs =
  case decode (SL.fromChunks [bs]) of
    Left msg -> putStrLn msg
    Right req -> do
      mrsp <- handleRequest conf req
      case mrsp of
        Just rsp ->
            let packet = mconcat . SL.toChunks $ encode rsp
            in  void $ timeout (3 * 1000 * 1000) (sendAllTo sock packet addr)
        Nothing -> return ()

-- | a default server: handle queries for ever
defaultServer :: ServerConf -> Socket -> IO ()
defaultServer conf sock =
  forever $ do
    (bs, addr) <- recvFrom sock (bufSize conf)
    forkIO $ handleQuery conf sock addr bs
