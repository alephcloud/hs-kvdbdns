-- |
-- Module      : Network.DNS.API.Client
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DNS.API.Client
    ( sendQuery
    , sendQueryRaw
    , sendQueryDefault
    , sendQueryRawDefault
    , sendQueryDefaultTo
    , sendQueryRawDefaultTo
    , makeResolvSeedSafe
      -- * Other needed types
    , PortNumber
    , DNS.ResolvSeed
    ) where

import Control.Exception
import Control.Monad.Except

import Data.Byteable
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Hourglass.Types

import qualified Network.DNS as DNS
import Network.DNS.API.Types
import Network.DNS.API.Utils
import Network.Socket (PortNumber)

tryAny :: IO a -> (SomeException -> IO a) -> IO a
tryAny = Control.Exception.catch

-- | Parse the response
parseQuery :: Packable response
           => [DNS.RDATA]
           -> Dns (Response response)
parseQuery q =
  decodeResponse . B.concat =<< mapM toByteString q
  where
    toByteString :: DNS.RDATA -> Dns ByteString
    toByteString (DNS.RD_TXT bs) = return bs
    toByteString d               = errorDns $ "unexpected type: " ++ (show d)

-- | Send a Raw query
sendQueryRaw :: Packable response
             => DNS.Resolver
             -> FQDN
             -> DnsIO (Response response)
sendQueryRaw resolver fqdn = do
    r <- doLookup $ toBytes fqdn
    pureDns $ parseQuery r
  where
    doLookup :: DNS.Domain -> DnsIO [DNS.RDATA]
    doLookup d = do
      result <- liftIO $ tryAny (DNS.lookup resolver d DNS.TXT) -- :: IO (Either DNS.DNSError [DNS.RDATA])
                                (\_ -> return $ Left DNS.OperationRefused)
      case result of
        Left err -> pureDns $ errorDns $ "DNS api: sendQuery: " ++ (show err)
        Right l  -> return l

-- | Send a query
sendQuery :: (Packable response, Encodable request)
          => DNS.Resolver
          -> request
          -> FQDN
          -> DnsIO (Response response)
sendQuery res q dom =
  case execDns (encode q >>= \e -> appendFQDN e dom) of
    Left err -> pureDns $ errorDns err
    Right e -> sendQueryRaw res e

------------------------------------------------------------------------------

-- | Send a raw query with the given resolvSeed
sendQueryRawDefaultTo :: (Packable response)
                      => DNS.ResolvSeed -- ^ the DNSResolverSeed
                      -> FQDN
                      -> DnsIO (Response response)
sendQueryRawDefaultTo seed fqdn = do
   ret <- liftIO $ DNS.withResolver seed $ \resolver ->
                      execDnsIO $ sendQueryRaw resolver fqdn
   case ret of
     Left err -> errorDnsIO err
     Right a  -> return a

-- | Send a TXT query with the given resolvSeed
sendQueryDefaultTo :: (Packable response, Encodable request)
                   => DNS.ResolvSeed -- ^ the DNSResolverSeed
                   -> request
                   -> FQDN
                   -> DnsIO (Response response)
sendQueryDefaultTo seed req dom = do
   ret <- liftIO $ DNS.withResolver seed $ \resolver ->
                     execDnsIO $ sendQuery resolver req dom
   case ret of
     Left err -> errorDnsIO err
     Right a  -> return a

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

-- | send a Raw query using the default resolver
sendQueryRawDefault :: Packable response
                    => FQDN
                    -> DnsIO (Response response)
sendQueryRawDefault fqdn = do
    rs <- liftIO $ DNS.makeResolvSeed DNS.defaultResolvConf
    sendQueryRawDefaultTo rs fqdn
-- | Send a TXT query with the default DNS Resolver
--
-- Equivalent to:
-- @
--  sendQueryDefault dom = do
--    rs <- DNS.makeResolvSeed DNS.defaultResolvConf
--    DNS.withResolver rs $
--           \resolver -> sendQuery resolver dom
-- @
sendQueryDefault :: (Packable response, Encodable request)
                 => request
                 -> FQDN
                 -> DnsIO (Response response)
sendQueryDefault req dom = do
    rs <- liftIO $ DNS.makeResolvSeed DNS.defaultResolvConf
    sendQueryDefaultTo rs req dom
