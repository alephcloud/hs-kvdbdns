-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Network.DNS.API.Types
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

module Network.DNS.API.Types
  ( -- ** Class
    Encodable(..)
  , decodeFQDNEncoded
  , encodeFQDNEncoded
  , dnsParse
    -- * Response
    -- ** Class
  , Packable(..)
  , packData
  , unpackData
  ) where

import qualified Codec.Binary.Base32    as BSB32
import Data.Byteable
import Data.ByteString (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Parse  as BP
import Data.Char (toUpper, toLower)

import Network.DNS.API.Error
import Network.DNS.API.FQDN
import Network.DNS.API.Packer

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
decodeFQDNEncoded :: (Encodable encodable, FQDN fqdn)
                  => fqdn
                  -> Dns encodable
decodeFQDNEncoded fqdn = decode32FQDNEncoded fqdn >>= dnsParse decode

-- | encode
encodeFQDNEncoded :: (Encodable encodable, FQDN fqdn)
                  => encodable
                  -> Dns fqdn
encodeFQDNEncoded d = runDnsPacker (encode d) >>= encode32FQDNEncoded

-- | Encode into a FQDN compatible fornat
encode32FQDNEncoded :: (FQDN fqdn)
                    => ByteString
                    -> Dns fqdn
encode32FQDNEncoded bs = fromNodes nodeList
  where
    nodeList :: [Node]
    nodeList = splitByNode $ replacePadding $ BSB32.encode bs

    splitByNode :: ByteString -> [Node]
    splitByNode b
      | (B.length b) < 63 = [Node b]
      | otherwise         = (Node node):(splitByNode xs)
      where
        (node, xs) = B.splitAt 63 b

-- | base 32 decode a FQDN
-- but do not decode the Request
decode32FQDNEncoded :: FQDN fqdn
                    => fqdn
                    -> Dns ByteString
decode32FQDNEncoded fqdn =
    case BSB32.decode $ B.concat nodeList of
        Left  _   -> errorDns "unable to decode (from base 32) the given FQDN."
        Right dbs -> return dbs
  where
    nodeList :: [ByteString]
    nodeList = map (resetPadding . toBytes) $ splitToNodes fqdn

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
