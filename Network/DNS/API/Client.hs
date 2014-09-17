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
      -- * Other needed types
    , PortNumber
    , DNS.ResolvSeed
    ) where

import Network.DNS.API.Types
import Network.DNS.API.Utils
import Network.Socket (PortNumber)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import qualified Network.DNS as DNS

import Control.Exception
import Control.Monad.Except
import Data.Functor.Identity

tryAny :: IO a -> (SomeException -> IO a) -> IO a
tryAny = Control.Exception.catch

-- | Parse the response
parseQuery :: Packable p
           => [DNS.RDATA]
           -> Dns (Response p)
parseQuery q =
  decodeResponse . B.concat =<< mapM toByteString q
  where
    toByteString :: DNS.RDATA -> Dns ByteString
    toByteString (DNS.RD_TXT bs) = return bs
    toByteString d               = throwError $ "unexpected type: " ++ (show d)

-- | Send a TXT query with the given DNS Resolver
sendQuery :: (Packable p, Encodable a)
          => DNS.Resolver
          -> a            -- ^ the key/request
          -> DnsIO (Response p)
sendQuery res q = do
  case runIdentity $ runExceptT (encode q >>= checkEncoding) of
    Left err -> throwError err
    Right e -> do
      r <- doLookup e
      either (throwError) (return) $ runIdentity $ runExceptT $ parseQuery r
  where
    doLookup :: DNS.Domain -> DnsIO [DNS.RDATA]
    doLookup d = do
      result <- liftIO $ tryAny (DNS.lookup res d DNS.TXT) -- :: IO (Either DNS.DNSError [DNS.RDATA])
                                (\_ -> return $ Left DNS.OperationRefused)
      case result of
        Left err -> throwError $ "DNS api: sendQuery: " ++ (show err)
        Right l  -> return l

------------------------------------------------------------------------------

-- | Send a TXT query with the given resolvSeed
sendQueryDefaultTo :: (Packable p, Encodable a)
                   => DNS.ResolvSeed -- ^ the DNSResolverSeed
                   -> a              -- ^ the request
                   -> DnsIO (Response p)
sendQueryDefaultTo seed req = do
   ret <- liftIO $ DNS.withResolver seed $ \resolver ->
                     runExceptT $ sendQuery resolver req
   case ret of
     Left err -> throwError err
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
                   -> Maybe Int        -- ^ timeout
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
                     r2 = maybe r1 (\to -> r1 { DNS.resolvTimeout = to }) mto
                 in maybe r2 (\r  -> r2 { DNS.resolvRetry = r }) mr

-- | Send a TXT query with the default DNS Resolver
--
-- Equivalent to:
-- @
--  sendQueryDefault dom = do
--    rs <- DNS.makeResolvSeed DNS.defaultResolvConf
--    DNS.withResolver rs $
--           \resolver -> sendQuery resolver dom
-- @
sendQueryDefault :: (Packable p, Encodable a)
                 => a   -- the request
                 -> DnsIO (Response p)
sendQueryDefault req = do
    rs <- liftIO $ DNS.makeResolvSeed DNS.defaultResolvConf
    sendQueryDefaultTo rs req
