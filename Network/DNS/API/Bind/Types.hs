-- |
-- Module      : Network.DNS.API.Bind.Types
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@pivotmail.com>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE RankNTypes #-}
module Network.DNS.API.Bind.Types
    ( BindingFunction(..)
    , BindingA
    , BindingAAAA
    , BindingTXT
    , BindingPTR
    , BindingNS
    , BindingCNAME
    , BindingDNAME
      -- * Option bindings
    , Opts
    , emptyOpts
    , insertOpts
    , withSafeOpt
    , withUnsafeOpt
    ) where

import           Data.ByteString (ByteString)
import           Data.IP (IPv4, IPv6)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Network.DNS.API.Connection
import           Network.DNS.API.Error
import           Network.DNS.API.FQDN

-------------------------------------------------------------------------------
--                              Binding                                      --
-------------------------------------------------------------------------------

-- | The function to trigger for a given FQDN+recordType
newtype BindingFunction value = BindingFunction
    { bindingFunction :: forall context . Connection context -> ValidFQDN -> DnsIO value
    }

type BindingA      = BindingFunction [IPv4]
type BindingAAAA   = BindingFunction [IPv6]
type BindingTXT    = BindingFunction [ByteString]
type BindingPTR    = BindingFunction [ValidFQDN]
type BindingNS     = BindingFunction [ValidFQDN]
type BindingCNAME  = BindingFunction [ValidFQDN]
type BindingDNAME  = BindingFunction [ValidFQDN]

-------------------------------------------------------------------------------
--                              Binding options                              --
-------------------------------------------------------------------------------

newtype Opts = Opts
    { getOpts :: Map String String
    } deriving (Show, Eq)

insertOpts :: String -> String -> Opts -> Opts
insertOpts k v m = Opts $ Map.insert k v (getOpts m)

emptyOpts :: Opts
emptyOpts = Opts Map.empty

withSafeOpt :: Opts -> String -> (Maybe String -> a) -> a
withSafeOpt opts k f = f $ Map.lookup k $ getOpts opts

withUnsafeOpt :: Opts -> String -> (String -> a) -> a
withUnsafeOpt opts k f =
    case Map.lookup k $ getOpts opts of
        Nothing -> error $ "Network.DNS.API.Bind: expected options: " ++ k
        Just v  -> f v
