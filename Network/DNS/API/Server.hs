-- |
-- Module      : Network.DNS.API.Server
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DNS.API.Server
  ( -- * Helpers
    ServerConf(..)
  , createServerConf
  , handleRequest
  , Connection(getContext, getSockAddr, getCreationDate, getLastUsedDate, setKeepOpen)
    -- * defaultServer
  , getDefaultConnections
  , defaultServer
    -- * Create DNSFormat
  , defaultQuery
  , defaultResponse
  ) where

import Control.Monad
import Control.Applicative

import Data.ByteString (ByteString)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as SL (toChunks, fromChunks)
import Data.Maybe
import Data.Monoid (mconcat)

import Network.DNS hiding (lookup)
import qualified Network.DNS.API.Types as API
import Network.DNS.API.Connection (Connection)
import qualified Network.DNS.API.Connection as API
import Network.Socket hiding (recvFrom, recv, send)

import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan

------------------------------------------------------------------------------
--                         Server Configuration                             --
------------------------------------------------------------------------------

-- | Server configuration
data API.Packable p => ServerConf context p = ServerConf
  { query :: Connection context
          -> ByteString
          -> IO (Maybe (API.Response p)) -- ^ the method to perform a request
  , inFail :: DNSFormat
           -> IO (Either String DNSFormat) -- ^ the method to use to handle query failure
  }

-- | Smart constructor for DNS API configuration
--
-- Use this function instead of the default one:
-- > let conf = def :: ServerConf
-- this method will only refuse every DNS query and will return an error Code : ServFail
--
-- you need to replace the @query@ method. The best way to use it is to use this function
createServerConf :: API.Packable p
                 => (Connection a -> ByteString -> IO (Maybe (API.Response p)))
                 -> ServerConf a p
createServerConf function =
   ServerConf
      { query   = function
      , inFail  = inFailError
      }

-- | Default implementation of an inFail
inFailError :: DNSFormat -> IO (Either String DNSFormat)
inFailError req =
  let hd = header req
      flg = flags hd
  in  return $ Right $ req { header = hd { flags = flg { qOrR = QR_Response
                                                       , rcode = ServFail
                                                       }
                                         }
                           }

------------------------------------------------------------------------------
--                         The main function                                --
------------------------------------------------------------------------------

splitTxt :: ByteString -> [ByteString]
splitTxt bs
  | B.length bs < 255 = [bs]
  | otherwise = node:(splitTxt xs)
  where
    (node, xs) = B.splitAt 255 bs

-- Handle a request:
-- try the query function given in the ServerConf
-- if it fails, then call the given proxy
handleRequest :: API.Packable p => ServerConf a p -> Connection a -> DNSFormat -> IO (Either String ByteString)
handleRequest conf conn req =
  case listToMaybe . filterTXT . question $ req of
    Just q -> do
        mres <- query conf conn $ qname q
        case mres of
           Just txt -> return $ Right $ mconcat . SL.toChunks $ encode $ responseTXT q (splitTxt $ API.encodeResponse txt)
           Nothing  -> inFail conf req >>= return.inFailWrapper
    Nothing -> inFail conf req >>= return.inFailWrapper
  where
    filterTXT = filter ((==TXT) . qtype)

    ident :: Int
    ident = identifier . header $ req

    inFailWrapper :: Either String DNSFormat -> Either String ByteString
    inFailWrapper (Right r) = Right $ mconcat . SL.toChunks $ encode r
    inFailWrapper (Left er) = Left er

    responseTXT :: Question -> [ByteString] -> DNSFormat
    responseTXT q l =
      let hd = header defaultResponse
          dom = qname q
          al = map (\txt -> ResourceRecord dom TXT 0 (B.length txt) (RD_TXT txt)) l
      in  defaultResponse
            { header = hd { identifier = ident, qdCount = 1, anCount = length al }
            , question = [q]
            , answer = al
            }

-- | imported from dns:Network/DNS/Internal.hs
--
-- use this to get a default DNS format to send a query (if needed)
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

-- | imported from dns:Network/DNS/Internal.hs
--
-- use this to get a default DNS format to send a response
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

------------------------------------------------------------------------------
--                          Internal Queue System                           --
------------------------------------------------------------------------------

data DNSReqToHandle a = DNSReqToHandle
    { connection :: Connection a
    , getReq     :: DNSFormat
    }

type DNSReqToHandleChan a = TChan (DNSReqToHandle a)

newDNSReqToHandleChan :: IO (DNSReqToHandleChan a)
newDNSReqToHandleChan = atomically $ newTChan

putReqToHandle :: (DNSReqToHandleChan a) -> (DNSReqToHandle a) -> IO ()
putReqToHandle chan req = atomically $ writeTChan chan req

popReqToHandle :: (DNSReqToHandleChan a) -> IO (DNSReqToHandle a)
popReqToHandle = atomically . readTChan

------------------------------------------------------------------------------
--                         Default server: helpers                          --
------------------------------------------------------------------------------

defaultQueryHandler :: API.Packable p => ServerConf a p -> DNSReqToHandleChan a -> IO ()
defaultQueryHandler conf chan = do
  dnsReq <- popReqToHandle chan
  eResp <- handleRequest conf (connection dnsReq) (getReq dnsReq)
  case eResp of
    Right bs -> defaultResponder (connection dnsReq) bs
    Left err -> putStrLn err
  where
    defaultResponder :: Connection a -> ByteString -> IO ()
    defaultResponder conn resp = do
       API.write conn resp
       keepOpen <- API.getKeepOpen conn
       if keepOpen
         then API.close conn -- TODO: put these requests into a forkIO
         else API.close conn

-- | a default server: handle queries for ever
defaultListener :: DNSReqToHandleChan a -> Connection a -> IO ()
defaultListener chan conn = do
  -- Listen the given connection
  API.listen conn 10
  -- TODO: for now block it to no more than 10 connections
  -- start accepting connection:
  forever $ do
    -- wait to get some request
    client <- API.accept conn
    -- read data
    mbs <- API.read client 512
    case mbs of
      Nothing -> API.close client -- in case of a timeout: ignore the connection
      Just bs ->
        -- Try to decode it, if it works then add it to the queue
        case decode (SL.fromChunks [bs]) of
          Left  _   -> API.close client -- We don't want to throw an error if the command is wrong
          Right req -> putReqToHandle chan $ DNSReqToHandle client req

-- | Simple helper to get the default DNS Sockets
--
-- all sockets TCP/UDP + IPv4 + port(53)
getDefaultConnections :: (Monad m, Applicative m)
                      => Maybe String
                      -> Int -- ^ time out configuration for the connections read/write
                      -> Maybe a -- ^ the initial context value to use to the created connections
                      -> IO [m (Connection a)]
getDefaultConnections mport timeout mcontext = do
  let (mflags, service) = maybe (([], Just "domain")) (\port -> ([AI_NUMERICSERV], Just port)) mport
  addrinfos <- getAddrInfo
                   (Just (defaultHints
                            { addrFlags = AI_PASSIVE:mflags
                            , addrFamily = AF_INET
                            }
                         )
                   )
                   (Nothing)
                   service
  mapM (addrInfoToSocket timeout mcontext) addrinfos

addrInfoToSocket :: (Monad m, Applicative m) => Int -> Maybe a -> AddrInfo -> IO (m (Connection a))
addrInfoToSocket timeout mcontext addrinfo
  | (addrSocketType addrinfo) `notElem` [Datagram, Stream] = return $ fail $ "socket type not supported: " ++ (show addrinfo)
  | otherwise = do
      sock <- socket (addrFamily addrinfo) (addrSocketType addrinfo) defaultProtocol
      bindSocket sock (addrAddress addrinfo)
      case addrSocketType addrinfo of
           Datagram -> API.newConnectionUDPServer sock timeout mcontext >>= return.pure
           Stream   -> API.newConnectionTCPServer sock timeout mcontext >>= return.pure
           _        -> return $ fail $ "Socket Type not handle: " ++ (show addrinfo)

defaultServer :: API.Packable p
              => ServerConf a p
              -> [Connection a]
              -> IO ()
defaultServer _    []       = error $ "Network.DNS.API.Server: defaultServer: list of DNSApiConnection is empty"
defaultServer conf sockList = do
  -- creat a TChan to pass request from the listeners to the handler
  chan <- newDNSReqToHandleChan
  -- start the listerners
  mapM_ (forkIO . forever . defaultListener chan) sockList
  -- start the query Hander
  forever $ defaultQueryHandler conf chan
