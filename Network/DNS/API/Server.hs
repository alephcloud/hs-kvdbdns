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
  , handleRequest
    -- * defaultServer
  , DNSAPIConnection
  , getDefaultSockets
  , defaultListener
  , defaultServer
  ) where

import Control.Monad (forever, void)
import System.Timeout

import Data.ByteString (ByteString)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as SL (toChunks, fromChunks)
import Data.Default
import Data.Maybe  (listToMaybe)
import Data.Monoid (mconcat)

import Network.DNS hiding (lookup)
import qualified Network.DNS.API.Types as API
import Network.Socket hiding (recvFrom, recv)
import Network.Socket.ByteString (sendAllTo, recvFrom)

import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan

------------------------------------------------------------------------------
--                         Server Configuration                             --
------------------------------------------------------------------------------

-- | Server configuration
data ServerConf = ServerConf
  { query   :: SockAddr -> ByteString -> IO (Maybe API.Response) -- ^ the method to perform a request
  , inFail  :: DNSFormat -> IO (Either String DNSFormat) -- ^ the method to use to handle query failure
  }

-- Default
instance Default ServerConf where
    def = ServerConf
      { query   = queryNothing
      , inFail  = inFailError
      }

-- | Default implementation of a query
queryNothing :: SockAddr -> ByteString -> IO (Maybe API.Response)
queryNothing _ _ = return Nothing

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
handleRequest :: ServerConf -> SockAddr -> DNSFormat -> IO (Either String ByteString)
handleRequest conf addr req =
  case listToMaybe . filterTXT . question $ req of
    Just q -> do
        mres <- query conf addr $ qname q
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

------------------------------------------------------------------------------
--                          Internal Queue System                           --
------------------------------------------------------------------------------

data DNSReqToHandle = DNSReqToHandle
    { connection :: DNSAPIConnection
    , sender     :: SockAddr
    , getReq     :: DNSFormat
    }

type DNSReqToHandleChan = TChan DNSReqToHandle

newDNSReqToHandleChan :: IO DNSReqToHandleChan
newDNSReqToHandleChan = atomically $ newTChan

putReqToHandle :: DNSReqToHandleChan -> DNSReqToHandle -> IO ()
putReqToHandle chan req = atomically $ writeTChan chan req

popReqToHandle :: DNSReqToHandleChan -> IO DNSReqToHandle
popReqToHandle = atomically . readTChan

------------------------------------------------------------------------------
--                         Default server: helpers                          --
------------------------------------------------------------------------------

data DNSAPIConnection
    = UDPConnection Socket
    deriving (Show, Eq)

defaultQueryHandler :: ServerConf -> DNSReqToHandleChan -> IO ThreadId
defaultQueryHandler conf chan =
  forkIO $ forever $ do
    dnsReq <- popReqToHandle chan
    eResp <- handleRequest conf (sender dnsReq) (getReq dnsReq)
    case eResp of
      Right bs -> defaultResponder dnsReq bs
      Left err -> putStrLn err
  where
    defaultResponder :: DNSReqToHandle -> ByteString -> IO ()
    defaultResponder req resp =
      case connection req of
        UDPConnection sock -> void $ timeout (3 * 1000 * 1000) (sendAllTo sock resp (sender req))

-- | a default server: handle queries for ever
defaultListener :: DNSReqToHandleChan -> DNSAPIConnection -> IO ThreadId
defaultListener chan (UDPConnection sock) =
  -- accept all requests, and then send the raw received bytestring to the queue
  -- in order to decode it and then treat it
  forkIO $ forever $ do
    -- wait to get some request
    (bs, addr) <- recvFrom sock 512
    -- Try to decode it, if it works then add it to the queue
    case decode (SL.fromChunks [bs]) of
      Left msg  -> error $ "DNS.decode: Error: " ++ msg
      Right req -> putReqToHandle chan $ DNSReqToHandle (UDPConnection sock) addr req

getDefaultSockets :: IO [DNSAPIConnection]
getDefaultSockets = do
  addrinfos <- getAddrInfo
                   (Just (defaultHints
                            { addrFlags = [AI_PASSIVE]
                            , addrFamily = AF_INET
                            , addrSocketType = Datagram
                            }
                         )
                   )
                   (Nothing)
                   (Just "domain")
  mapM addrInfoToSocket addrinfos
  where
    addrInfoToSocket :: AddrInfo -> IO DNSAPIConnection
    addrInfoToSocket addrinfo = do
      sock <- socket (addrFamily addrinfo) (addrSocketType addrinfo) defaultProtocol
      bindSocket sock (addrAddress addrinfo)
      return $ case addrSocketType addrinfo of
                    Datagram -> UDPConnection sock
                    _        -> error $ "Socket Type not handle: " ++ (show addrinfo)

defaultServer :: ServerConf -> IO (ThreadId, [ThreadId])
defaultServer conf = do
  chan <- newDNSReqToHandleChan
  defaultSocketList <- getDefaultSockets
  listThreadListeners <- mapM (defaultListener chan) defaultSocketList
  threadHandler <- defaultQueryHandler conf chan
  return (threadHandler, listThreadListeners)
