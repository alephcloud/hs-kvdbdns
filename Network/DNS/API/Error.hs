-- |
-- Module      : Network.DNS.API.Error
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.DNS.API.Error
    ( Dns
    , DnsIO
    , errorDns
    , errorDnsIO
    , pureDns
    , execDns
    , execDnsIO
    ) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity

-- | Pure DNS error Monad
newtype Dns   a = Dns { runDns :: Except String a }
    deriving (Functor, Applicative, Monad)
-- | DNS error IO Monad
newtype DnsIO a = DnsIO { runDnsIO :: ExceptT String IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

-- | lift a Dns into a DnsIO
pureDns :: Dns a -> DnsIO a
pureDns = (either (DnsIO . throwError) return) . execDns

-- | throw a Dns error
errorDns :: String -> Dns a
errorDns = Dns . throwError

-- | run the Dns
execDns :: Dns a -> Either String a
execDns = runExcept . runDns

-- | throw a DnsIO error
errorDnsIO :: String -> DnsIO a
errorDnsIO = DnsIO . throwError

-- | run the DnsIO
execDnsIO :: DnsIO a -> IO (Either String a)
execDnsIO = runExceptT . runDnsIO
