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
import qualified Data.ByteString.Char8 as BC
import Data.Hourglass.Types

import qualified Network.DNS as DNS
import Network.DNS.API.Error
import Network.DNS.API.Types
import Network.DNS.API.FQDN
import Network.Socket (PortNumber, HostName)

------------------------------------------------------------------------------
-- Internal methods
------------------------------------------------------------------------------

-- catch all exception
catchAny :: IO a -> (SomeException -> DnsIO a) -> DnsIO a
catchAny action alternativeAction = do
    res <- liftIO $ tryAny action
    case res of
        Left  e -> alternativeAction e
        Right a -> return a

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
                 => DNS.TYPE
                 -> fqdn
                 -> DnsIO [DNS.RDATA]
sendQueryRawType t fqdn = do
    rs <- liftIO $ DNS.makeResolvSeed DNS.defaultResolvConf { DNS.resolvInfo = DNS.RCHostName "8.8.8.8" }
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

-- | This function is to contact a specific DNS Resolver using a Domain Name
contactDNSResolver :: FQDN fqdn
                   => fqdn             -- ^ the DNS Server to contact
                   -> Maybe PortNumber -- ^ port number
                   -> Maybe Seconds    -- ^ timeout
                   -> Maybe Int        -- ^ retry
                   -> DnsIO DNS.ResolvSeed
contactDNSResolver fqdn mport mto mr = do
    rs <- catchAny
            (DNS.makeResolvSeed resolvConf)
            (\ex -> pureDns $ errorDns $ errorMsg $ "error while creating the ResolvSeed: " ++ show ex)
    (tryLookupWith rs DNS.lookupA) <|> (tryLookupWith rs DNS.lookupAAAA)
  where
    errorMsg :: String -> String
    errorMsg str = "Network.DNS.API.Client.contactDNSResovler: " ++ str

    resolvConf :: DNS.ResolvConf
    resolvConf = let r1 = DNS.defaultResolvConf { DNS.resolvInfo = DNS.RCHostName "8.8.8.8" }
                     r2 = maybe r1 (\(Seconds to) -> r1 { DNS.resolvTimeout = (fromIntegral to) * 3000 * 3000 }) mto
                 in maybe r2 (\r  -> r2 { DNS.resolvRetry = r }) mr

    tryLookupWith :: (Show output)
                  => DNS.ResolvSeed
                  -> (DNS.Resolver -> DNS.Domain -> IO (Either DNS.DNSError [output]))
                  -> DnsIO DNS.ResolvSeed
    tryLookupWith rs lookupFunction = do
        result <- liftIO $ DNS.withResolver rs $ \resolver -> lookupFunction resolver (toBytes fqdn)
        case result of
            Right [] -> pureDns $ errorDns $ errorMsg $ "no server to contact at this domain: " ++ (show $ toBytes fqdn)
            Right l  -> tryUntilSuccess
                            (\a -> contactDNSResolverAt (show a) mport mto mr)
                            (pureDns $ errorDns $ errorMsg "non of the IPv4 address worked for this domain " ++ (show $ toBytes fqdn))
                            l
            Left err -> pureDns $ errorDns $ errorMsg $ "error while lookupA: " ++ show err

-- | Attempt to create a resolv seed which contact the given hostname
contactDNSResolverAt :: HostName         -- ^ an ip address (IPv6 or IPv4)
                     -> Maybe PortNumber -- ^ port number
                     -> Maybe Seconds    -- ^ timeout
                     -> Maybe Int        -- ^ retry
                     -> DnsIO DNS.ResolvSeed
contactDNSResolverAt hostname mport mto mr =
    catchAny
         (DNS.makeResolvSeed $ resolvConf { DNS.resolvInfo = resolvInfo })
         (\ex -> pureDns $ errorDns $ errorMsg $ "could not create a resolv seed from the received IP: " ++ show hostname ++ " error is: " ++ show ex)
  where
    errorMsg :: String -> String
    errorMsg str = "Network.DNS.API.Client.contactDNSResovlerAt: " ++ str

    resolvInfo :: DNS.FileOrNumericHost
    resolvInfo = maybe (DNS.RCHostName hostname) (\p -> DNS.RCHostPort hostname p) mport
    resolvConf :: DNS.ResolvConf
    resolvConf = let r1 = DNS.defaultResolvConf { DNS.resolvInfo = DNS.RCHostName "8.8.8.8" }
                     r2 = maybe r1 (\(Seconds to) -> r1 { DNS.resolvTimeout = (fromIntegral to) * 3000 * 3000 }) mto
                 in maybe r2 (\r  -> r2 { DNS.resolvRetry = r }) mr

-- | Create the ResolvSeed using the given parameters
--
-- Will first attempt to create a ResolvSeed with the HostName information
-- (The DNS Server address and the port) if provided.
-- We will try any IPv4 or IPv6 available from the given domain name
--
-- If still not found, we will then fall back to the default DNS Resolver
-- (/etc/resolv.conf in linux/mac).
--
-- This function raises an exception in case the last attempt failed
makeResolvSeedSafe :: Maybe ByteString -- ^ the DNS Server to contact (Domain Name, IPv4 or IPv6)
                   -> Maybe PortNumber -- ^ port number (useless without the DNS Server to contact)
                   -> Maybe Seconds    -- ^ timeout
                   -> Maybe Int        -- ^ retry
                   -> IO DNS.ResolvSeed
makeResolvSeedSafe mfqdn mport mto mr = do
    mrs <- maybe (return $ Left "no Domain Name provided, fall back to the local resolv configuration")
                 (\fqdn -> execDnsIO $ (contactDNSResolverAt (BC.unpack fqdn) mport mto mr) <|> (contactDNSResolver fqdn mport mto mr))
                 mfqdn
    case mrs of
        Right rs -> return rs
        Left  _  -> DNS.makeResolvSeed resolvConf
  where
    resolvConf :: DNS.ResolvConf
    resolvConf = let r1 = DNS.defaultResolvConf { DNS.resolvInfo = DNS.RCHostName "8.8.8.8" }
                     r2 = maybe r1 (\(Seconds to) -> r1 { DNS.resolvTimeout = (fromIntegral to) * 3000 * 3000 }) mto
                 in maybe r2 (\r  -> r2 { DNS.resolvRetry = r }) mr
