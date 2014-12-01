-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Network.DNS.API
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

module Network.DNS.API
  ( -- * Types
    -- ** FQDN
    FQDN(..)
  , Node(..)
  , ValidFQDN
  , UnsafeFQDN(..)
  , validateFQDN
  , unsafeValidFQDN
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
    -- ** Packable
  , Packable(..)
  , packData
  , unpackData
  , encodeFQDNEncoded
  , decodeFQDNEncoded

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
import Network.DNS.API.FQDN
import Network.DNS.API.Client
import Network.DNS.API.Server
