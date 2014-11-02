-- |
-- Module      : Network.DNS.API.Types
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
module Network.DNS.API.Types
  ( -- * Request
    -- ** FQDN encoding
    FQDNEncoded
  , FQDN
  , encodeFQDN
  , unsafeToFQDN
    -- ** Class
  , Encodable(..)
  , decodeFQDNEncoded
  , encodeFQDNEncoded
  , dnsParse
    -- * Response
    -- ** Class
  , Packable(..)
  , packData
  , unpackData
  ) where

import Control.Applicative

import qualified Codec.Binary.Base32    as BSB32
import Data.Byteable
import Data.ByteString (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Parse  as BP
import Data.Char (toUpper, toLower)

import Network.DNS.API.Error
import Network.DNS.API.Packer

------------------------------------------------------------------------------
--                                FQDN                                      --
------------------------------------------------------------------------------

-- | represent a encoded but not validated FQDN
-- (means that this FQDN is base32 encoded and but may not be a valide FQDN
newtype FQDNEncoded = FQDNEncoded ByteString
    deriving (Show, Eq, Ord)

instance Byteable FQDNEncoded where
    toBytes (FQDNEncoded bs) = bs

-- | build a encoded FQDN
encodeFQDN :: ByteString -> FQDNEncoded
encodeFQDN bs = FQDNEncoded bs

-- | represent a valide FQDN
newtype FQDN = FQDN ByteString
    deriving (Show, Eq, Ord)

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

------------------------------------------------------------------------------
--                                Encodable                                 --
------------------------------------------------------------------------------

-- | This is the main type to implement to make your requests encodable
--
-- As we use the Domain Name field to send request to the DNS Server we need to
-- encode the URL into a format that will be a valide format for every DNS
-- servers our request may go through.
class Encodable encodable where
    encode :: encodable -> DnsPacker
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
encodeFQDNEncoded d = runDnsPacker (encode d) >>= encode32FQDNEncoded

-- | Encode into a FQDN compatible fornat
encode32FQDNEncoded :: ByteString
                    -> Dns FQDNEncoded
encode32FQDNEncoded bs = encodeFQDN <$> fqdn
  where
    fqdn :: Dns ByteString
    fqdn = return . B.intercalate (BC.pack ['.']) . splitByNode $ replacePadding $ BSB32.encode bs

    splitByNode :: ByteString -> [ByteString]
    splitByNode b
      | (B.length b) < 63 = [b]
      | otherwise = node:(splitByNode xs)
      where
        (node, xs) = B.splitAt 63 b

replacePadding :: ByteString -> ByteString
replacePadding bs = BC.map filterPadding bs
  where
    filterPadding :: Char -> Char
    filterPadding '=' = '9'
    filterPadding c   = toLower c

resetPadding :: ByteString -> ByteString
resetPadding bs = BC.map filterPadding bs
  where
    filterPadding :: Char -> Char
    filterPadding '9' = '='
    filterPadding c   = toUpper c

-- | base 32 decode a FQDN
-- but do not decode the Request
decode32FQDNEncoded :: FQDNEncoded
                    -> Dns ByteString
decode32FQDNEncoded fqdn =
    case BSB32.decode $ B.concat $ BC.split '.' $ resetPadding bs of
        Left  _   -> errorDns "unable to decode (from base 32) the given FQDN."
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
    pack   :: packable -> DnsPacker
    unpack :: BP.Parser packable

-- | unpack the given bytestring
unpackData :: Packable packable
           => ByteString
           -> Dns packable
unpackData = dnsParse unpack

-- | pack the given data
packData :: Packable packable => packable -> Dns ByteString
packData = runDnsPacker . pack
