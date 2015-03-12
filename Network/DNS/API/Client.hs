-- Copyright (c) 2013-2015 PivotCloud, Inc.
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
    , contactDNSResolver
    , contactDNSResolverAt
    , makeResolvSeedSafe
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Except

import Data.Byteable
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import qualified Network.DNS as DNS
import Network.DNS.API.Error
import Network.DNS.API.Types
import Network.DNS.API.FQDN
import Network.DNS.API.Resolv

------------------------------------------------------------------------------
-- Internal methods
------------------------------------------------------------------------------

tryAny :: IO a -> IO (Either SomeException a)
tryAny = Control.Exception.try

sendQueryRawType' :: DNS.TYPE
                  -> DNS.Resolver
                  -> ValidFQDN
                  -> DnsIO [DNS.RDATA]
sendQueryRawType' t resolver fqdn = do
    doLookup $ toBytes fqdn
  where
    errorMsg :: String -> String
    errorMsg = (++) "Network.DNS.API.Client.sendQueryRaw: "

    doLookup :: DNS.Domain -> DnsIO [DNS.RDATA]
    doLookup d = do
        result <- liftIO $ tryAny (DNS.lookup resolver d t)
        case result of
            Left ex -> pureDns $ errorDns $ errorMsg $ "exception while trying to contact server: " ++ show ex
            Right (Left err) -> pureDns $ errorDns $ errorMsg $ "server respond error message: " ++ show err
            Right (Right l) -> return l

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
                 => DNS.ResolvConf -- ^ The resolver configuration
                 -> DNS.TYPE
                 -> fqdn
                 -> DnsIO [DNS.RDATA]
sendQueryRawType rc t fqdn = do
    rs <- getFirstResolvSeed [rc]
    sendQueryRawToType t rs fqdn

-- | send a Raw query using the default resolver
sendQueryRaw :: FQDN fqdn
             => DNS.ResolvConf
             -> fqdn
             -> DnsIO [DNS.RDATA]
sendQueryRaw rc = sendQueryRawType rc DNS.TXT

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
    rs <- makeResolvSeedSafe Nothing Nothing Nothing Nothing
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
    rs <- makeResolvSeedSafe Nothing Nothing Nothing Nothing
    rep <- sendQueryToType DNS.TXT rs req dom
    ret <- pureDns $ parseQueryTXT rep
    pureDns $ unpackData ret
