-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Network.DNS.API.Error
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
