-- |
-- Module      : Network.DNS.KVDB.Server
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE OverloadedStrings #-}
module Network.DNS.KVDB.Server
  ( ServerConf(..)
  , handleQuery
  ) where

import Control.Applicative ((<$>))
import Control.Monad (void)

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
  { query   :: ByteString -> IO (Maybe ByteString) -- ^ the method to perform a request
  , inFail  :: DNSFormat  -> IO (Either String DNSFormat) -- ^ the method to use to handle query failure
  }

-- Default
instance Default ServerConf where
    def = ServerConf
      { query   = queryJustEcho
      , inFail  = inFailUndefined
      }

-- | Default implementation of a query
-- it returns the received key (expecting a Dummy query)
queryJustEcho :: ByteString -> IO (Maybe ByteString)
queryJustEcho req = return $ Just $ S.pack $ funFromString $ KVDB.key request
  where
    request :: KVDB.Dummy
    request = KVDB.decode req
    funFromString :: [Char] -> [Word8]
    funFromString = map (fromIntegral.ord)

-- | Default implementation of an inFail
inFailUndefined :: DNSFormat -> IO (Either String DNSFormat)
inFailUndefined _ = return $ Left "command undefined"

------------------------------------------------------------------------------
--                         The main function                                --
------------------------------------------------------------------------------

-- Handle a request:
-- try the query function given in the ServerConf
-- if it fails, then call the given proxy
handleRequest :: ServerConf -> DNSFormat -> IO (Either String ByteString)
handleRequest conf req =
  case listToMaybe . filterTXT . question $ req of
    Just q -> do
        mres <- query conf $ qname q
        case mres of
           Just txt -> return $ Right $ mconcat . SL.toChunks $ encode $ responseTXT q txt
           Nothing  -> inFail conf req >>= return.inFailWrapper
    Nothing -> inFail conf req >>= return.inFailWrapper
  where
    filterTXT = filter ((==TXT) . qtype)

    ident :: Int
    ident = identifier . header $ req

    inFailWrapper :: Either String DNSFormat -> Either String ByteString
    inFailWrapper (Right r) = Right $ mconcat . SL.toChunks $ encode r
    inFailWrapper (Left er) = Left er

    responseTXT :: Question -> ByteString -> DNSFormat
    responseTXT q txt =
      let hd = header defaultResponse
          dom = qname q
          an = ResourceRecord dom TXT 0 (S.length txt) (RD_TXT txt)
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
            -> ByteString -- ^ the query
            -> IO (Either String ByteString)
handleQuery conf bs =
  case decode (SL.fromChunks [bs]) of
    Left msg  -> return $ Left $ "Query: Error: " ++ msg
    Right req -> handleRequest conf req
