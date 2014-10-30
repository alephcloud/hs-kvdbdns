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
  ( -- * Types
    ServerConf(..)
  , createServerConf
  , Connection(getContext, getSockAddr, getCreationDate, getLastUsedDate, setKeepOpen)
    -- * defaultServer
  , getDefaultConnections
  , defaultServer
  ) where

import Control.Monad
import Control.Applicative

import Data.Byteable
import Data.ByteString (ByteString)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as SL (toChunks, fromChunks)
import Data.Hourglass.Types
import Data.IP
import Data.Monoid (mconcat)

import Network.DNS hiding (encode, decode, lookup, responseA, responseAAAA)
import qualified Network.DNS as DNS
import Network.DNS.API.Types
import Network.DNS.API.Error
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
data ServerConf context = ServerConf
  { queryA     :: Connection context -> FQDNEncoded -> DnsIO [IPv4]
  , queryAAAA  :: Connection context -> FQDNEncoded -> DnsIO [IPv6]
  , queryTXT   :: Connection context -> FQDNEncoded -> DnsIO ByteString
  , queryNS    :: Connection context -> FQDNEncoded -> DnsIO [FQDN]
  , queryCNAME :: Connection context -> FQDNEncoded -> DnsIO [FQDN]
  , queryDNAME :: Connection context -> FQDNEncoded -> DnsIO [FQDN]
  , queryPTR   :: Connection context -> FQDNEncoded -> DnsIO [FQDN]
  , inFail :: DNSFormat -> IO (Either String DNSFormat)
  }

-- | Smart constructor for DNS API configuration
--
-- Will set the TXT handler for you
-- and will configure the other to always fail properly
createServerConf :: (Connection a -> FQDNEncoded -> DnsIO ByteString)
                 -> ServerConf a
createServerConf function =
   ServerConf
      { queryA     = recordNotImplemented
      , queryAAAA  = recordNotImplemented
      , queryTXT   = function
      , queryNS    = recordNotImplemented
      , queryCNAME = recordNotImplemented
      , queryDNAME = recordNotImplemented
      , queryPTR   = recordNotImplemented
      , inFail     = return . Right . failError
      }
  where
    recordNotImplemented :: Connection context -> FQDNEncoded -> DnsIO a
    recordNotImplemented _ _ = pureDns $ errorDns "Record not implemented"

failError :: DNSFormat -> DNSFormat
failError req =
  let hd = header req
      flg = flags hd
  in  req { header = hd { flags = flg { qOrR = QR_Response
                                      , rcode = ServFail
                                      }
                        }
          }

------------------------------------------------------------------------------
--                         The main function                                --
------------------------------------------------------------------------------

-- Handle a request:
-- try the query function given in the ServerConf
-- if it fails, then call the given proxy
handleRequest :: ServerConf a -> Connection a -> DNSFormat -> IO (Either String ByteString)
handleRequest conf conn req = do
    r <- case qtype q of
            DNS.A     -> handleRequestA    $ queryA     conf conn fqdn
            DNS.AAAA  -> handleRequestAAAA $ queryAAAA  conf conn fqdn
            DNS.TXT   -> handleRequestTXT  $ queryTXT   conf conn fqdn
            DNS.NS    -> handleRequestFQDN DNS.NS    $ queryNS    conf conn fqdn
            DNS.CNAME -> handleRequestFQDN DNS.CNAME $ queryCNAME conf conn fqdn
            DNS.DNAME -> handleRequestFQDN DNS.DNAME $ queryDNAME conf conn fqdn
            DNS.PTR   -> handleRequestFQDN DNS.PTR   $ queryPTR   conf conn fqdn
            _         -> either error id <$> inFail conf req
    return $ Right $ mconcat $ SL.toChunks $ DNS.encode r
  where
    q :: Question
    q = head $ question req
    fqdn :: FQDNEncoded
    fqdn = encodeFQDN $ qname q

    ident :: Int
    ident = identifier . header $ req

    handleRequestFQDN :: DNS.TYPE -> DnsIO [FQDN] -> IO DNSFormat
    handleRequestFQDN t action = do
        mres <- execDnsIO action
        return $ case mres of
            Left  err -> failError req
            Right dn  -> responseFQDN q ident t dn

    handleRequestA :: DnsIO [IPv4] -> IO DNSFormat
    handleRequestA action = do
        mres <- execDnsIO action
        return $ case mres of
            Left  err -> failError req
            Right l   -> responseA q ident l


    handleRequestAAAA :: DnsIO [IPv6] -> IO DNSFormat
    handleRequestAAAA action = do
        mres <- execDnsIO action
        return $ case mres of
            Left  err -> failError req
            Right l   -> responseAAAA q ident l

    handleRequestTXT :: DnsIO ByteString -> IO DNSFormat
    handleRequestTXT action = do
        mres <- execDnsIO action
        return $ case mres of
            Left  err -> failError req
            Right bs  -> responseTXT q ident (splitTxt bs)

responseFQDN :: Question -> Int -> DNS.TYPE -> [FQDN] -> DNSFormat
responseFQDN q ident t l =
    let hd = header defaultResponse
        dom = qname q
        al = map (\fqdn -> ResourceRecord dom t 0 (f fqdn) (helper fqdn)) l
    in  defaultResponse
            { header = hd { identifier = ident, qdCount = 1, anCount = length al }
            , question = [q]
            , answer = al
            }
  where
    f :: FQDN -> Int
    f fqdn = B.length $ toBytes fqdn
    helper :: FQDN -> DNS.RD a
    helper fqdn = case t of
        DNS.NS    -> DNS.RD_NS    $ toBytes fqdn
        DNS.CNAME -> DNS.RD_CNAME $ toBytes fqdn
        DNS.DNAME -> DNS.RD_DNAME $ toBytes fqdn
        DNS.PTR   -> DNS.RD_PTR   $ toBytes fqdn
        _         -> error $ "cannot build an ResponseFQDN with type: " ++ show t
responseA :: Question -> Int -> [IPv4] -> DNSFormat
responseA q ident l =
    let hd = header defaultResponse
        dom = qname q
        al = map (\ip -> ResourceRecord dom DNS.A 0 4 (RD_A ip)) l
    in  defaultResponse
            { header = hd { identifier = ident, qdCount = 1, anCount = length al }
            , question = [q]
            , answer = al
            }
responseAAAA :: Question -> Int -> [IPv6] -> DNSFormat
responseAAAA q ident l =
    let hd = header defaultResponse
        dom = qname q
        al = map (\ip -> ResourceRecord dom DNS.AAAA 0 16 (RD_AAAA ip)) l
    in  defaultResponse
            { header = hd { identifier = ident, qdCount = 1, anCount = length al }
            , question = [q]
            , answer = al
            }

responseTXT :: Question -> Int -> [ByteString] -> DNSFormat
responseTXT q ident l =
  let hd = header defaultResponse
      dom = qname q
      al = map (\txt -> ResourceRecord dom TXT 0 (B.length txt) (RD_TXT txt)) l
  in  defaultResponse
        { header = hd { identifier = ident, qdCount = 1, anCount = length al }
        , question = [q]
        , answer = al
        }

splitTxt :: ByteString -> [ByteString]
splitTxt bs
  | B.length bs < 255 = [bs]
  | otherwise = node:(splitTxt xs)
  where
    (node, xs) = B.splitAt 255 bs

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

defaultQueryHandler :: ServerConf a -> DNSReqToHandleChan a -> IO ()
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
        case DNS.decode (SL.fromChunks [bs]) of
          Left  _   -> API.close client -- We don't want to throw an error if the command is wrong
          Right req -> putReqToHandle chan $ DNSReqToHandle client req

-- | Simple helper to get the default DNS Sockets
--
-- all sockets TCP/UDP + IPv4 + port(53)
getDefaultConnections :: (Monad m, Applicative m)
                      => Maybe String
                      -> Seconds -- ^ for timeout
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

addrInfoToSocket :: (Monad m, Applicative m) => Seconds -> Maybe a -> AddrInfo -> IO (m (Connection a))
addrInfoToSocket timeout mcontext addrinfo
  | (addrSocketType addrinfo) `notElem` [Datagram, Stream] = return $ fail $ "socket type not supported: " ++ (show addrinfo)
  | otherwise = do
      sock <- socket (addrFamily addrinfo) (addrSocketType addrinfo) defaultProtocol
      bindSocket sock (addrAddress addrinfo)
      case addrSocketType addrinfo of
           Datagram -> API.newConnectionUDPServer sock timeout mcontext >>= return.pure
           Stream   -> API.newConnectionTCPServer sock timeout mcontext >>= return.pure
           _        -> return $ fail $ "Socket Type not handle: " ++ (show addrinfo)

-- | launch the default server
defaultServer :: ServerConf a
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
