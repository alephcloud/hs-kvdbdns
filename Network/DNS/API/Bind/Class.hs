-- |
-- Module      : Network.DNS.API.Bind.Class
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@pivotmail.com>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DNS.API.Bind.Class
    ( Binding(..)
    , DefaultBinding(..)
    , notImplementedBinding
    ) where

import           Data.Byteable
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.IP (IPv4, IPv6)
import           Network.DNS.API.Bind.Types
import           Network.DNS.API.Connection (Connection)
import           Network.DNS.API.Error
import           Network.DNS.API.FQDN

class Binding binding where
    -- The function to use to parse a 
    getName   :: binding -> String

    getA      :: binding -> Opts -> Dns BindingA
    getAAAA   :: binding -> Opts -> Dns BindingAAAA
    getTXT    :: binding -> Opts -> Dns BindingTXT
    getPTR    :: binding -> Opts -> Dns BindingPTR
    getNS     :: binding -> Opts -> Dns BindingNS
    getCNAME  :: binding -> Opts -> Dns BindingCNAME
    getDNAME  :: binding -> Opts -> Dns BindingDNAME

-- | In case you don't want to implement an interface you can provide this function
notImplementedBinding :: Binding binding => binding -> Opts -> Dns (BindingFunction a)
notImplementedBinding _ _ = return $ BindingFunction $ \_ _ -> pureDns $ errorDns "error: not implemented"

-------------------------------------------------------------------------------
--                              Default Binding                              --
-------------------------------------------------------------------------------

data DefaultBinding = DefaultBinding
    deriving (Show, Eq)

instance Binding DefaultBinding where
    getName _ = "default"

    getA _ opts =
        withUnsafeOpt opts "ip" $ \str ->
            let ip = read str :: IPv4
            in  return $ BindingFunction $ defaultBindingReturn [ip]
    getAAAA _ opts =
        withUnsafeOpt opts "ip" $ \str ->
            let ip = read str :: IPv6
            in  return $ BindingFunction $ defaultBindingReturn [ip]
    getTXT _ opts =
        withUnsafeOpt opts "text" $ \str ->
            let bs = BC.pack str :: ByteString
            in  return $ BindingFunction $ defaultBindingReturn [bs]
    getPTR   _ = defaultBindingFQDN
    getNS    _ = defaultBindingFQDN
    getCNAME _ = defaultBindingFQDN
    getDNAME _ = defaultBindingFQDN

checkFQDNEmpty :: FQDN fqdn => fqdn -> Bool
checkFQDNEmpty fqdn
    | byteableLength fqdn == 0 = True
    | otherwise                = False

defaultBindingReturn :: value -> Connection context -> ValidFQDN -> DnsIO value
defaultBindingReturn v _ fqdn
    | checkFQDNEmpty fqdn = return v
    | otherwise           = pureDns $ errorDns "error: void binding"

defaultBindingFQDN :: Opts -> Dns (BindingFunction [ValidFQDN])
defaultBindingFQDN opts =
    withUnsafeOpt opts "fqdn" $ \str -> do
        fqdn <- validateFQDN $ BC.pack str
        return $ BindingFunction $ defaultBindingReturn [fqdn]
