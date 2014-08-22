-- |
-- Module      : Network.DNS.KVDB.Client
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DNS.KVDB.Client
    ( sendQuery
    , sendQueryDefault
    ) where

import Network.DNS.KVDB.Types
import Network.DNS.KVDB.Utils

import Data.ByteString.Char8 (ByteString)

import qualified Network.DNS as DNS

-- | Send a TXT query with the given DNS Resolver
sendQuery :: Encodable a
          => DNS.Resolver
          -> a            -- ^ the key/request
          -> IO (Either DNS.DNSError [DNS.RDATA])
sendQuery res q
  | checkEncoding dom = DNS.lookup res dom DNS.TXT 
  | otherwise         = return $ Left DNS.IllegalDomain
  where
    dom :: DNS.Domain
    dom = encode q

-- | Send a TXT query with the default DNS Resolver
--
-- Equivalent to:
-- @
--  sendQueryDefault dom = do
--    rs <- DNS.makeResolvSeed DNS.defaultResolvConf
--    DNS.withResolver rs $
--           \resolver -> sendQuery resolver dom
-- @
sendQueryDefault :: Encodable a
                 => a
                 -> IO (Either DNS.DNSError [DNS.RDATA])
sendQueryDefault dom = do
    rs <- DNS.makeResolvSeed DNS.defaultResolvConf
    DNS.withResolver rs $ \resolver -> sendQuery resolver dom
