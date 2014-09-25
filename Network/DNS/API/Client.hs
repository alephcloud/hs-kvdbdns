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
    ( -- * Send queries
      -- ** main methods
      sendQuery
    , sendQueryTo
      -- ** raw methods
    , sendQueryRaw
    , sendQueryRawTo
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
import Network.DNS.API.Types
import Network.DNS.API.Utils
import Network.Socket (PortNumber)

------------------------------------------------------------------------------
-- Internal methods
------------------------------------------------------------------------------

-- catch all exception
tryAny :: IO a -> (SomeException -> IO a) -> IO a
tryAny = Control.Exception.catch

-- | Parse the response
parseQuery :: [DNS.RDATA]
           -> Dns ByteString
parseQuery q =
    -- decodeResponse .
    B.concat <$> mapM toByteString q
  where
    toByteString :: DNS.RDATA -> Dns ByteString
    toByteString (DNS.RD_TXT bs) = return bs
    toByteString d               = errorDns $ "unexpected type: " ++ (show d)

-- | Send a Raw query
sendQueryRaw' :: DNS.Resolver
              -> FQDN
              -> DnsIO ByteString
sendQueryRaw' resolver fqdn = do
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

------------------------------------------------------------------------------
-- DNS Query
------------------------------------------------------------------------------

-- Raw queries ---------------------------------------------------------------

-- | Send a raw query with the given resolvSeed
sendQueryRawTo :: DNS.ResolvSeed -- ^ the DNSResolverSeed
               -> FQDN
               -> DnsIO ByteString
sendQueryRawTo seed fqdn = do
   ret <- liftIO $ DNS.withResolver seed $ \resolver ->
                      execDnsIO $ sendQueryRaw' resolver fqdn
   case ret of
     Left err -> errorDnsIO err
     Right a  -> return a

-- | send a Raw query using the default resolver
sendQueryRaw :: FQDN
             -> DnsIO ByteString
sendQueryRaw fqdn = do
    rs <- liftIO $ DNS.makeResolvSeed DNS.defaultResolvConf
    sendQueryRawTo rs fqdn

-- Generic queries -----------------------------------------------------------

-- | Send a TXT query with the given resolvSeed
sendQueryTo :: (Packable response, Encodable request)
            => DNS.ResolvSeed -- ^ the DNSResolverSeed
            -> request
            -> FQDN
            -> DnsIO response
sendQueryTo seed req dom = do
    fqdn <- pureDns $ encodeFQDNEncoded req >>= \e -> appendFQDN e dom
    ret <- sendQueryRawTo seed fqdn
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
sendQuery :: (Packable response, Encodable request)
          => request
          -> FQDN
          -> DnsIO response
sendQuery req dom = do
    rs <- liftIO $ makeResolvSeedSafe Nothing Nothing Nothing Nothing
    sendQueryTo rs req dom

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

