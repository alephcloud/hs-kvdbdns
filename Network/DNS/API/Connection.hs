-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Network.DNS.API.Connection
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

module Network.DNS.API.Connection
    ( Connection (..)
    , newConnectionUDPServer
    , newConnectionTCPServer
    ) where

import Data.ByteString (ByteString)
import Prelude hiding (read)
import Network.Socket (Socket, SockAddr)
import qualified Network.Socket as Socket (close, accept, listen)
import qualified Network.Socket.ByteString as Socket
import Control.Concurrent.MVar

import System.Timeout (timeout)
import Data.Hourglass
import System.Hourglass

-- | Represent a client connection (UDP or TCP)
data Connection a = Connection
  { listen      :: Int -> IO ()
  , accept      :: IO (Connection a)
  , read        :: Int -> IO (Maybe ByteString)
  , write       :: ByteString -> IO ()
  , close       :: IO ()
    -- Aimed to be use at any time:
  , getKeepOpen :: IO Bool          -- ^ does the session need to be kept opened
  , setKeepOpen :: Bool -> IO ()    -- ^ set the current session to keep is open (or not)
  , getContext  :: MVar a           -- ^ a connection context
  , getSockAddr :: SockAddr         -- ^ the connection socket address
  , getCreationDate :: ElapsedP     -- ^ the UNIX timestamp creation date
  , getLastUsedDate :: IO ElapsedP  -- ^ the UNIX timestamp last use date
  }

updateTimeStamp :: MVar ElapsedP -> IO ()
updateTimeStamp mvar = do
  time <- timeCurrentP
  modifyMVar_ mvar (\_ -> return time)

-- | Create a new UDP Connection server with the given ttl and context.
--
-- In this case it is better to not use the context since a UDP connection
-- is suppose to be a very volatile connection (we can't maintain a connection
-- with a client).
--
-- If an initial context is provided, it will be also used in any accepted Connections
newConnectionUDPServer :: Socket  -- ^ the socket to wrap up in a Connection
                       -> Seconds -- ^ Timeout in any Read/Write actions
                       -> Maybe a -- ^ default context: will be use for every accepted connections
                       -> IO (Connection a)
newConnectionUDPServer sock ttl mc = do
  mvar <- maybe (newEmptyMVar) (newMVar) mc
  date <- timeCurrentP
  lastUse <- newMVar date
  return $ Connection
    { listen  = \_ -> return ()
    , accept  = acceptUDPClient sock ttl mc >>= \r -> (updateTimeStamp lastUse >> return r)
    , read    = error "Network.DNS.API.Connection.UDP.Server: should not read"
    , write   = error "Network.DNS.API.Connection.UDP.Server: should not write"
    , close   = Socket.close sock >> updateTimeStamp lastUse
    , getKeepOpen = return True
    , setKeepOpen = \_ -> return ()
    , getContext = mvar
    , getSockAddr = error "Network.DNS.API.Connection.UDP.Server: do not provide sock addr"
    , getCreationDate = date
    , getLastUsedDate = readMVar lastUse
    }

acceptUDPClient :: Socket
                -> Seconds
                -> Maybe a
                -> IO (Connection a)
acceptUDPClient sock (Seconds s) mc = do
  let ttl = (fromIntegral s) * 1000 * 1000
  mvar <- maybe (newEmptyMVar) (newMVar) mc
  (bs, addr) <- Socket.recvFrom sock 512
  date <- timeCurrentP
  lastUse <- newMVar date
  return $ Connection
    { listen  = error "Network.DNS.API.Connection.UDP.Client: should not listen"
    , accept  = error "Network.DNS.API.Connection.UDP.Client: should not accept"
    , read    = \_ -> updateTimeStamp lastUse >> (return $ Just bs)
    , write   = \b -> (timeout ttl $ Socket.sendAllTo sock b addr) >> updateTimeStamp lastUse
    , close   = updateTimeStamp lastUse -- the given socket is the same than the server Socket, don't close it
    , getKeepOpen = return False -- In a case of a UDP Connection we don't want to keep it open
    , setKeepOpen = \_ -> return ()
    , getContext = mvar
    , getSockAddr = addr
    , getCreationDate = date
    , getLastUsedDate = readMVar lastUse
    }

-- | Create a new Connection for TCP Server
newConnectionTCPServer :: Socket  -- ^ the socket to wrap up in a Connection
                       -> Seconds -- ^ ttl
                       -> Maybe a -- ^ an initial value to use in the context of the udp requests
                       -> IO (Connection a)
newConnectionTCPServer sock ttl mc = do
  mvar <- maybe (newEmptyMVar) (newMVar) mc
  date <- timeCurrentP
  lastUse <- newMVar date
  return $ Connection
    { listen  = \qSize -> Socket.listen sock qSize >> updateTimeStamp lastUse
    , accept  = acceptTCPClient sock ttl mc >>= (\r -> updateTimeStamp lastUse >> return r)
    , read    = error "Network.DNS.API.Connection.TCP.Server: should not read"
    , write   = error "Network.DNS.API.Connection.TCP.Server: should not write"
    , close   = Socket.close sock >> updateTimeStamp lastUse
    , getKeepOpen = return True
    , setKeepOpen = \_ -> return ()
    , getContext = mvar
    , getSockAddr = error "Network.DNS.API.Connection.UDP.Server: do not provide sock addr"
    , getCreationDate = date
    , getLastUsedDate = readMVar lastUse
    }

acceptTCPClient :: Socket
                -> Seconds
                -> Maybe a
                -> IO (Connection a)
acceptTCPClient sock (Seconds s) mc = do
  let ttl = (fromIntegral s) * 1000 * 1000
  mvar <- maybe (newEmptyMVar) (newMVar) mc
  -- by default we don't want to keep opened connections
  keepOpen <- newMVar False
  (sockClient, addr) <- Socket.accept sock
  date <- timeCurrentP
  lastUse <- newMVar date
  return $ Connection
    { listen  = error "Network.DNS.API.Connection.TCP.Client: should not listen"
    , accept  = error "Network.DNS.API.Connection.TCP.Client: should not accept"
    , read    = \size -> (timeout ttl $ Socket.recv sockClient size) >>= (\r -> updateTimeStamp lastUse >> return r)
    , write   = \bs   -> (timeout ttl $ Socket.send sockClient bs) >> updateTimeStamp lastUse
    , close   = Socket.close sockClient >> updateTimeStamp lastUse
    , getKeepOpen = readMVar keepOpen
    , setKeepOpen = \b -> modifyMVar_ keepOpen (\_ -> return b)
    , getContext = mvar
    , getSockAddr = addr
    , getCreationDate = date
    , getLastUsedDate = readMVar lastUse
    }
