-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Network.DNS.API.Server
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
--

module Network.DNS.API.Server
    ( -- * Types
      ServerConf(..)
    , createServerConf
    , Connection(getContext, getSockAddr, getCreationDate, getLastUsedDate, setKeepOpen)
      -- * defaultServer
    , getDefaultConnections
    , defaultServer
      -- * helper
    , failError
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Monad.STM

import           Data.Byteable
import           Data.ByteString (ByteString)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as SL (toChunks, fromChunks)
import           Data.Hourglass.Types
import           Data.IP
import           Data.Maybe (catMaybes)
import           Data.Monoid (mconcat)

import           Network.DNS hiding (encode, decode, lookup, responseA, responseAAAA)
import qualified Network.DNS as DNS

import           Network.DNS.API.Bind
import           Network.DNS.API.Error
import           Network.DNS.API.FQDN
import           Network.DNS.API.Connection (Connection)
import qualified Network.DNS.API.Connection as API
import           Network.Socket hiding (recvFrom, recv, send)

------------------------------------------------------------------------------
--                         Server Configuration                             --
------------------------------------------------------------------------------

-- | Server configuration
data ServerConf context = ServerConf
    { getBindings :: DNSBindings
    , inFail      :: DNSFormat -> IO (Either String DNSFormat)
    }

-- | Create a default Server Conf with every queries configured to
-- fail properly
createServerConf :: DNSBindings -- ^ the collection of bindings
                 -> ServerConf context
createServerConf b =
    ServerConf
        { getBindings = b
        , inFail   = return . Right . failError
        }

-- | Create the right DNSFormat error to answer to a query which failed
failError :: DNSFormat -- ^ the original DNS Format
          -> DNSFormat
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
    case execDns $ validateFQDN $ qname q of
        Left err   -> return $ Left err
        Right fqdn -> do
            r <- case qtype q of
                    DNS.A     -> handleRequestA              (bindingsA     $ getBindings conf) fqdn
                    DNS.AAAA  -> handleRequestAAAA           (bindingsAAAA  $ getBindings conf) fqdn
                    DNS.TXT   -> handleRequestTXT            (bindingsTXT   $ getBindings conf) fqdn
                    DNS.NS    -> handleRequestFQDN DNS.NS    (bindingsPTR   $ getBindings conf) fqdn
                    DNS.CNAME -> handleRequestFQDN DNS.CNAME (bindingsCNAME $ getBindings conf) fqdn
                    DNS.DNAME -> handleRequestFQDN DNS.DNAME (bindingsDNAME $ getBindings conf) fqdn
                    DNS.PTR   -> handleRequestFQDN DNS.PTR   (bindingsPTR   $ getBindings conf) fqdn
                    _         -> either error id <$> inFail conf req
            return $ Right $ mconcat $ SL.toChunks $ DNS.encode r
  where
    q :: Question
    q = head $ question req

    ident :: Int
    ident = identifier . header $ req

    handleRequestFQDN :: DNS.TYPE -> Bindings [ValidFQDN] -> ValidFQDN -> IO DNSFormat
    handleRequestFQDN t b fqdn =
        case execDns $ findBinding fqdn b of
            Left _err             -> return $ failError req
            Right (action, param) -> do
                mres <- execDnsIO $ bindingFunction action conn param
                return $ case mres of
                    Left  _err-> failError req
                    Right dn  -> responseFQDN q ident t dn

    handleRequestA :: BindingsA -> ValidFQDN -> IO DNSFormat
    handleRequestA b fqdn = do
        case execDns $ findBinding fqdn b of
            Left _err             -> return $ failError req
            Right (action, param) -> do
                mres <- execDnsIO $ bindingFunction action conn param
                return $ case mres of
                    Left  _err-> failError req
                    Right l   -> responseA q ident l

    handleRequestAAAA :: BindingsAAAA -> ValidFQDN -> IO DNSFormat
    handleRequestAAAA b fqdn = do
        case execDns $ findBinding fqdn b of
            Left _err             -> return $ failError req
            Right (action, param) -> do
                mres <- execDnsIO $ bindingFunction action conn param
                return $ case mres of
                    Left  _err-> failError req
                    Right l   -> responseAAAA q ident l

    handleRequestTXT :: BindingsTXT -> ValidFQDN -> IO DNSFormat
    handleRequestTXT b fqdn = do
        case execDns $ findBinding fqdn b of
            Left  _err            -> return $ failError req
            Right (action, param) -> do
                mres <- execDnsIO $ bindingFunction action conn param
                return $ case mres of
                    Left  _err-> failError req
                    Right l   -> responseTXT q ident l

responseFQDN :: Question -> Int -> DNS.TYPE -> [ValidFQDN] -> DNSFormat
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
    f :: Byteable fqdn => fqdn -> Int
    f fqdn = B.length $ toBytes fqdn
    helper :: ValidFQDN -> DNS.RD a
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

-- | imported from dns:Network/DNS/Internal.hs
--
-- use this to get a default DNS format to send a query (if needed)
defaultQuery :: DNSFormat
defaultQuery =
    DNSFormat
        { header = DNSHeader
            { identifier = 0
            , flags = DNSFlags
                { qOrR         = QR_Query
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
    in  defaultQuery
            { header = hd
                { flags = flg
                    { qOrR = QR_Response
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
getDefaultConnections :: Maybe String
                      -> Seconds       -- ^ The timeout for every connections
                      -> Maybe context -- ^ A possible context every Connection can have (That could be a MVar or something else)
                      -> IO [Connection context]
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
    catMaybes <$> mapM (addrInfoToSocket timeout mcontext) addrinfos

addrInfoToSocket :: Seconds       -- ^ The timeout for every connections
                 -> Maybe context -- ^ A possible context every Connection can have (That could be a MVar or something else)
                 -> AddrInfo      -- ^ The address info
                 -> IO (Maybe (Connection context))
addrInfoToSocket timeout mcontext addrinfo
    | (addrSocketType addrinfo) `notElem` [Datagram, Stream] = return Nothing -- $ fail $ "socket type not supported: " ++ (show addrinfo)
    | otherwise = do
        sock <- socket (addrFamily addrinfo) (addrSocketType addrinfo) defaultProtocol
        bindSocket sock (addrAddress addrinfo)
        case addrSocketType addrinfo of
            Datagram -> Just <$> API.newConnectionUDPServer sock timeout mcontext
            Stream   -> Just <$> API.newConnectionTCPServer sock timeout mcontext
            _        -> return $ Nothing -- "Socket Type not handle: " ++ (show addrinfo)

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
