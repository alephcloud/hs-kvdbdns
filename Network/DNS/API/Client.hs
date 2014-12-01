-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Network.DNS.API.Client
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

module Network.DNS.API.Client
    ( -- * Send queries
      -- ** main methods
      sendQuery
    , sendQueryTo
    , sendQueryType
    , sendQueryToType
      -- ** raw methods
    , sendQueryRaw
    , sendQueryRawTo
    , sendQueryRawType
    , sendQueryRawToType
      -- * DNS resolver
    , makeResolvSeedSafe
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Except

import Data.Byteable
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Hourglass.Types

import qualified Network.DNS as DNS
import Network.DNS.API.Error
import Network.DNS.API.Types
import Network.DNS.API.FQDN
import Network.Socket (PortNumber)

------------------------------------------------------------------------------
-- Internal methods
------------------------------------------------------------------------------

-- catch all exception
tryAny :: IO a -> (SomeException -> IO a) -> IO a
tryAny = Control.Exception.catch

sendQueryRawType' :: DNS.TYPE
                  -> DNS.Resolver
                  -> ValidFQDN
                  -> DnsIO [DNS.RDATA]
sendQueryRawType' t resolver fqdn = do
    doLookup $ toBytes fqdn
  where
    doLookup :: DNS.Domain -> DnsIO [DNS.RDATA]
    doLookup d = do
      result <- liftIO $ tryAny (DNS.lookup resolver d t)
                                (\_ -> return $ Left DNS.OperationRefused)
      case result of
        Left err -> pureDns $ errorDns $ "Network.DNS.API.Client.sendQueryRaw': (" ++ (show fqdn) ++ "): " ++ (show err)
        Right l  -> return l

------------------------------------------------------------------------------
-- DNS Query
------------------------------------------------------------------------------

-- Raw queries ---------------------------------------------------------------

sendQueryRawToType :: FQDN fqdn
                   => DNS.TYPE
                   -> DNS.ResolvSeed -- ^ the DNSResolverSeed
                   -> fqdn
                   -> DnsIO [DNS.RDATA]
sendQueryRawToType t seed fqdn = do
    validFQDN <- pureDns $ validateFQDN fqdn
    ret <- liftIO $ DNS.withResolver seed $ \resolver ->
                        execDnsIO $ sendQueryRawType' t resolver validFQDN
    case ret of
        Left err -> errorDnsIO err
        Right a  -> return a

-- | Send a raw query with the given resolvSeed
sendQueryRawTo :: FQDN fqdn
               => DNS.ResolvSeed -- ^ the DNSResolverSeed
               -> fqdn
               -> DnsIO [DNS.RDATA]
sendQueryRawTo = sendQueryRawToType DNS.TXT

-- | send a Raw query using the default resolver
sendQueryRawType :: FQDN fqdn
                 => DNS.TYPE
                 -> fqdn
                 -> DnsIO [DNS.RDATA]
sendQueryRawType t fqdn = do
    rs <- liftIO $ DNS.makeResolvSeed DNS.defaultResolvConf
    sendQueryRawToType t rs fqdn

-- | send a Raw query using the default resolver
sendQueryRaw :: FQDN fqdn
             => fqdn
             -> DnsIO [DNS.RDATA]
sendQueryRaw = sendQueryRawType DNS.TXT

-- Generic queries -----------------------------------------------------------

-- | Send a @t@ record query to the the given DNS Resolver (seed)
sendQueryToType :: Packable request
                => DNS.TYPE
                -> DNS.ResolvSeed -- ^ the DNSResolverSeed
                -> request
                -> ValidFQDN
                -> DnsIO [DNS.RDATA]
sendQueryToType t seed req dom = do
    encoded <- pureDns $ encodeFQDNEncoded req :: DnsIO UnsafeFQDN
    fqdn <- pureDns $ appendFQDN encoded dom :: DnsIO ValidFQDN
    sendQueryRawToType t seed fqdn

-- | Send a query with the default DNS Resolver
--
-- Will send a record @type@ to the default DNS Resolver
sendQueryType :: Packable request
              => DNS.TYPE
              -> request
              -> ValidFQDN
              -> DnsIO [DNS.RDATA]
sendQueryType t req dom = do
    rs <- liftIO $ makeResolvSeedSafe Nothing Nothing Nothing Nothing
    sendQueryToType t rs req dom

-- | Parse the response
parseQueryTXT :: [DNS.RDATA]
              -> Dns ByteString
parseQueryTXT q =
    -- decodeResponse
    B.concat <$> mapM toByteString q
  where
    toByteString :: DNS.RDATA -> Dns ByteString
    toByteString (DNS.RD_TXT bs) = return bs
    toByteString d               = errorDns $ "unexpected type: " ++ (show d)

-- | Send a TXT query with the given resolvSeed
sendQueryTo :: (Packable response, Packable request)
            => DNS.ResolvSeed -- ^ the DNSResolverSeed
            -> request
            -> ValidFQDN
            -> DnsIO response
sendQueryTo seed req dom = do
    rep <- sendQueryToType DNS.TXT seed req dom
    ret <- pureDns $ parseQueryTXT rep
    pureDns $ unpackData ret

-- | Send a TXT query with the default DNS Resolver
--
-- Equivalent to:
-- @
--  sendQuery req dom = do
--    rs <- makeResolvSeedSafe Nothing Nothing Nothing Nothing
--    DNS.withResolver rs $
--           \resolver -> sendQueryTo resolver req dom
-- @
sendQuery :: (Packable response, Packable request)
          => request
          -> ValidFQDN
          -> DnsIO response
sendQuery req dom = do
    rs <- liftIO $ makeResolvSeedSafe Nothing Nothing Nothing Nothing
    rep <- sendQueryToType DNS.TXT rs req dom
    ret <- pureDns $ parseQueryTXT rep
    pureDns $ unpackData ret

------------------------------------------------------------------------------
-- ResolvSeed helper
------------------------------------------------------------------------------

-- | Create the ResolvSeed using the given parameters
--
-- Let Nothing to all the parameters to use the default options
-- If you want to contact directly a DNS Server, then this function
-- will first resolv its IPv4 Addr using the default DNS ResolvConf
--
-- If it fails to resolv the HostName, then this function return the
-- default Resolv Seed (with the given timout and retry options)
--
-- If you are using this function to connect to a DNS Server in the case
-- you have not Domain Name Service running on your machine: this function
-- may fail.
makeResolvSeedSafe :: Maybe ByteString -- ^ the DNS Server to contact
                   -> Maybe PortNumber -- ^ port number
                   -> Maybe Seconds    -- ^ timeout
                   -> Maybe Int        -- ^ retry
                   -> IO DNS.ResolvSeed
makeResolvSeedSafe mhn mport mto mr = do
  rs <- DNS.makeResolvSeed resolvConf
  case mhn of
    Nothing -> return rs
    Just hn -> do
      result <- DNS.withResolver rs $ \resolver -> DNS.lookupA resolver hn
      case result of
        Right (a:_) -> tryAny (DNS.makeResolvSeed $ resolvConf { DNS.resolvInfo = resolvInfo $ show a })
                              (\_ -> return rs)
        _           -> return rs
  where
    resolvInfo :: String -> DNS.FileOrNumericHost
    resolvInfo s = maybe (DNS.RCHostName s) (\p -> DNS.RCHostPort s p) mport
    resolvConf :: DNS.ResolvConf
    resolvConf = let r1 = DNS.defaultResolvConf
                     r2 = maybe r1 (\(Seconds to) -> r1 { DNS.resolvTimeout = (fromIntegral to) * 3000 * 3000 }) mto
                 in maybe r2 (\r  -> r2 { DNS.resolvRetry = r }) mr

