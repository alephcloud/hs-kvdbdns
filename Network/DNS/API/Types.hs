-- |
-- Module      : Network.DNS.API.Types
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.DNS.API.Types
  ( Dns
  , DnsIO
  , errorDns
  , errorDnsIO
  , pureDns
  , execDns
  , execDnsIO
    -- * Request
    -- ** FQDN encoding
  , FQDNEncoded
  , FQDN
  , encodeFQDN
  , unsafeToFQDN
    -- ** Class
  , Encodable(..)
  , decodeFQDNEncoded
  , encodeFQDNEncoded
    -- * Response
    -- ** Class
  , Packable(..)
  , packData
  , unpackData
  ) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity

import Data.Byteable
import Data.ByteString (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base32 as BSB32
import qualified Data.ByteString.Parse  as BP
import qualified Data.ByteString.Pack   as BP

------------------------------------------------------------------------------
--                               Error monad                                --
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
--                                FQDN                                      --
------------------------------------------------------------------------------

-- | represent a encoded but not validated FQDN
-- (means that this FQDN is base32 encoded and but may not be a valide FQDN
newtype FQDNEncoded = FQDNEncoded ByteString
    deriving (Show, Eq)

instance Byteable FQDNEncoded where
    toBytes (FQDNEncoded bs) = bs

-- | build a encoded FQDN
encodeFQDN :: ByteString -> FQDNEncoded
encodeFQDN bs = FQDNEncoded bs

-- | represent a valide FQDN
newtype FQDN = FQDN ByteString
    deriving (Show, Eq)

instance Byteable FQDN where
    toBytes (FQDN bs) = bs

-- | for FQDN
unsafeToFQDN :: FQDNEncoded -> FQDN
unsafeToFQDN = FQDN . toBytes

------------------------------------------------------------------------------
--                                   Helpers                                --
------------------------------------------------------------------------------

-- help to execute a parsing of a bytestring
dnsParse :: BP.Parser a -> ByteString -> Dns a
dnsParse parser bs = do
    l <- BP.parseFeed (return B.empty) parser bs
    case l of
        BP.ParseFail err -> errorDns $ "Network.DNS.API.Types.dnsParse: parse fail: " ++ err
        BP.ParseMore {}  -> errorDns "Network.DNS.API.Types.dnsParse: parse Partial"
        BP.ParseOK b v | B.null b  -> return v
                       | otherwise -> errorDns "Network.DNS.API.Types.dnsParse: unparsed data"

-- help to execute the packing of a bytestring
dnsPack :: (BP.Packer a, Int) -> Dns ByteString
dnsPack (packer, size) =
    let l = BP.pack packer size
    in  case l of
            Left err -> errorDns $ "Network.DNS.API.dnsPack: pack fail: " ++ err ++ " " ++ (show size)
            Right bs -> return bs

------------------------------------------------------------------------------
--                                Encodable                                 --
------------------------------------------------------------------------------

-- | This is the main type to implement to make your requests encodable
--
-- As we use the Domain Name field to send request to the DNS Server we need to
-- encode the URL into a format that will be a valide format for every DNS
-- servers our request may go through.
class Encodable encodable where
    encode :: encodable -> (BP.Packer (), Int) -- Dns FQDNEncoded
    decode :: BP.Parser encodable

-- | decode a FQDNEncoded
decodeFQDNEncoded :: Encodable encodable
                  => FQDNEncoded
                  -> Dns encodable
decodeFQDNEncoded fqdn = decode32FQDNEncoded fqdn >>= dnsParse decode

-- | encode
encodeFQDNEncoded :: Encodable encodable
                  => encodable
                  -> Dns FQDNEncoded
encodeFQDNEncoded d = dnsPack (encode d) >>= encode32FQDNEncoded

-- | Encode into a FQDN compatible fornat
encode32FQDNEncoded :: ByteString
                    -> Dns FQDNEncoded
encode32FQDNEncoded bs = encodeFQDN <$> fqdn
  where
    fqdn :: Dns ByteString
    fqdn = either (errorDns) (\e -> return $ B.intercalate (B.pack [0x2E]) $ splitByNode e) $ BSB32.encode bs

    splitByNode :: ByteString -> [ByteString]
    splitByNode b
      | (B.length b) < 63 = [b]
      | otherwise = node:(splitByNode xs)
      where
        (node, xs) = B.splitAt 63 b

-- | base 32 decode a FQDN
-- but do not decode the Request
decode32FQDNEncoded :: FQDNEncoded
                    -> Dns ByteString
decode32FQDNEncoded fqdn =
    case BSB32.decode $ B.concat $ B.split 0x2E bs of
        Left err  -> errorDns err
        Right dbs -> return dbs
  where
    bs :: ByteString
    bs = toBytes fqdn
------------------------------------------------------------------------------
--                              Packable                                    --
------------------------------------------------------------------------------

-- | This represent a packable
--
-- It is use to pack/unpack (into bytestring) a command in the case of the
-- proposed Request
class Packable packable where
    pack   :: packable -> (BP.Packer (), Int)
    unpack :: BP.Parser packable

-- | unpack the given bytestring
unpackData :: Packable packable
           => ByteString
           -> Dns packable
unpackData = dnsParse unpack

-- | pack the given data
packData :: Packable packable => packable -> Dns ByteString
packData = dnsPack . pack
