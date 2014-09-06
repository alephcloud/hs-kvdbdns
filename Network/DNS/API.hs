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
  ( -- * Class
    Encodable(..)
    -- * Client side
  , sendQuery
  , sendQueryDefault
    -- * Server side
  , ServerConf(..)
  , handleRequest
  ) where

import Network.DNS.API.Types

import Network.DNS.API.Client
import Network.DNS.API.Server
