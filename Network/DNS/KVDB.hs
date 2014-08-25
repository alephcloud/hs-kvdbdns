-- |
-- Module      : Network.DNS.KVDB
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE OverloadedStrings #-}
module Network.DNS.KVDB
  ( -- * Class
    Encodable(..)
    -- * Client side
  , sendQuery
  , sendQueryDefault
    -- * Server side
  , ServerConf(..)
  , handleQuery
  ) where

import Network.DNS.KVDB.Types

import Network.DNS.KVDB.Client
import Network.DNS.KVDB.Server
