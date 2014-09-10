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
    ) where

import Network.DNS.API.Types
import Network.DNS.API.Utils

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import qualified Network.DNS as DNS
import Control.Applicative

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
                 => a
                 -> IO (m (Response p))
sendQueryDefault dom = do
    rs <- DNS.makeResolvSeed DNS.defaultResolvConf
    DNS.withResolver rs $ \resolver -> sendQuery resolver dom
