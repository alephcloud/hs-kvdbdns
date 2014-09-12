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
    , sendQueryDefault
    , sendQueryDefaultTo
    , makeResolvSeedSafe
    ) where

import Network.DNS.API.Types
import Network.DNS.API.Utils

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import qualified Network.DNS as DNS
import Control.Applicative

-- | Parse the response
parseQuery :: (Monad m, Applicative m, Packable p)
           => [DNS.RDATA]
           -> m (Response p)
parseQuery q =
  decodeResponse . B.concat =<< mapM toByteString q
  where
    toByteString :: (Monad m, Applicative m) => DNS.RDATA -> m ByteString
    toByteString (DNS.RD_TXT bs) = pure bs
    toByteString d               = fail $ "unexpected type: " ++ (show d)

-- | Send a TXT query with the given DNS Resolver
sendQuery :: (Monad m, Applicative m, Packable p, Encodable a)
          => DNS.Resolver
          -> a            -- ^ the key/request
          -> IO (m (Response p))
sendQuery res q
  | checkEncoding' = do l <- dom >>= doLookup
                        l >>= return . parseQuery
  | otherwise      = return . fail $ show DNS.IllegalDomain
  where
    dom :: (Monad m, Applicative m) => m DNS.Domain
    dom = encode q

    checkEncoding' :: Bool
    checkEncoding' = maybe False checkEncoding dom

    doLookup :: (Monad m, Applicative m) => DNS.Domain -> IO (m [DNS.RDATA])
    doLookup d = do
      result <- DNS.lookup res d DNS.TXT :: IO (Either DNS.DNSError [DNS.RDATA])
      return $ case result of
        Left err -> fail $ show err
        Right l  -> pure l

------------------------------------------------------------------------------

-- | Send a TXT query with the given resolvSeed
sendQueryDefaultTo :: (Monad m, Applicative m, Packable p, Encodable a)
                   => DNS.ResolvSeed -- ^ the DNSResolverSeed
                   -> a              -- ^ the request
                   -> IO (m (Response p))
sendQueryDefaultTo seed req = do
  DNS.withResolver seed $ \resolver -> sendQuery resolver req

-- | Create the ResolvSeed using the given parameters
--
-- Let Nothing to all the parameter to use the default options
-- If you want to contact directly a DNS Server, then this function
-- will first resolv its IPv4 Addr using the default DNS ResolvConf
--
-- If it fails to resolv the HostName, then this function return the
-- default Resolv Seed (with the given timout and retry options)
makeResolvSeedSafe :: Maybe ByteString -- ^ the DNS Server to contact
                   -> Maybe Int        -- ^ timeout
                   -> Maybe Int        -- ^ retry
                   -> IO DNS.ResolvSeed
makeResolvSeedSafe mhn mto mr = do
  let resolvConf =
          let r1 = DNS.defaultResolvConf
              r2 = maybe r1 (\to -> r1 { DNS.resolvTimeout = to }) mto
          in maybe r2 (\r  -> r2 { DNS.resolvRetry = r }) mr
  rs <- DNS.makeResolvSeed resolvConf
  case mhn of
    Nothing -> return rs
    Just hn -> do
      result <- DNS.withResolver rs $ \resolver -> DNS.lookupA resolver hn
      case result of
        Right (a:_) -> DNS.makeResolvSeed $ resolvConf { DNS.resolvInfo = DNS.RCHostName $ show a }
        _           -> return rs

-- | Send a TXT query with the default DNS Resolver
--
-- Equivalent to:
-- @
--  sendQueryDefault dom = do
--    rs <- DNS.makeResolvSeed DNS.defaultResolvConf
--    DNS.withResolver rs $
--           \resolver -> sendQuery resolver dom
-- @
sendQueryDefault :: (Monad m, Applicative m, Packable p, Encodable a)
                 => a   -- the request
                 -> IO (m (Response p))
sendQueryDefault req = do
    rs <- DNS.makeResolvSeed DNS.defaultResolvConf
    sendQueryDefaultTo rs req
