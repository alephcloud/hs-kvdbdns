-- |
-- Module      : Network.DNS.API
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DNS.API
  ( -- * Types
    -- ** FQDN
    FQDN
  , FQDNEncoded
  , encodeFQDN
  , unsafeToFQDN
  , validateFQDN
  , appendFQDN
  , removeFQDNSuffix
    -- ** Errors
    -- *** pure
  , Dns
  , execDns
  , errorDns
    -- *** IO
  , DnsIO
  , pureDns
  , execDnsIO
  , errorDnsIO
    -- ** Request
  , Encodable(..)
  , encodeFQDNEncoded
  , decodeFQDNEncoded
    -- ** Response
  , Packable(..)
  , packData
  , unpackData

    -- * send DNS API queries
  , sendQuery
  , sendQueryTo

    -- * Accetp DNS API queries
    -- ** Types
  , Connection(getContext, getSockAddr, getCreationDate, getLastUsedDate, setKeepOpen)
  , ServerConf(..)
  , createServerConf
    -- ** helpers
  , getDefaultConnections
  , defaultServer
  ) where

import Network.DNS.API.Error
import Network.DNS.API.Types
import Network.DNS.API.Utils
import Network.DNS.API.Client
import Network.DNS.API.Server
