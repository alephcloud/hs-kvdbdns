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
  ( -- * Class
    Packable(..)
    -- ** helper
  , packData
  , unpackData
    -- ** FQDN Helper
  , decodeFQDNEncoded
  , encodeFQDNEncoded
    -- * level helpers
  , dnsParse
  , dnsPack
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

-- | help to execute a parsing of a bytestring
dnsParse :: BP.Parser a -- ^ the parser
         -> ByteString  -- ^ the bytestring to parse
         -> Dns a
dnsParse parser bs = do
    l <- BP.parseFeed (return B.empty) parser bs
    case l of
        BP.ParseFail err -> errorDns $ "Network.DNS.API.Types.dnsParse: parse fail: " ++ err
        BP.ParseMore {}  -> errorDns "Network.DNS.API.Types.dnsParse: parse Partial"
        BP.ParseOK b v | B.null b  -> return v
                       | otherwise -> errorDns "Network.DNS.API.Types.dnsParse: unparsed data"

------------------------------------------------------------------------------
--                              Packable                                    --
------------------------------------------------------------------------------

-- | The class that objects must implement in order to be embedded into DNS
-- queries or DNS responses.
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
packData = dnsPack . pack

------------------------------------------------------------------------------
--                            Pack / Unpack FQDN                            --
------------------------------------------------------------------------------

-- | decode a FQDN
--
-- Remove the dots, unbase32 and decode the bytestring
decodeFQDNEncoded :: (Packable packable, FQDN fqdn)
                  => fqdn         -- ^ the FQDN which contains the Encodable data
                  -> Dns packable
decodeFQDNEncoded fqdn = decode32FQDNEncoded >>= unpackData
  where
    decode32FQDNEncoded :: Dns ByteString
    decode32FQDNEncoded =
        case BSB32.decode $ resetPadding $ B.concat $ map toBytes $ toNodes fqdn of
            Left  _   -> errorDns "unable to decode (from base 32) the given FQDN."
            Right dbs -> return dbs

    resetPadding :: ByteString -> ByteString
    resetPadding bs = BC.map filterPadding bs
      where
        filterPadding :: Char -> Char
        filterPadding '9' = '='
        filterPadding c   = toUpper c

-- | encode into a FQDN
--
-- Run the Packer, base32 and intercalate a dot every 63 bytes
encodeFQDNEncoded :: (Packable packable, FQDN fqdn)
                  => packable -- ^ the data to encode into FQDN
                  -> Dns fqdn
encodeFQDNEncoded d = packData d >>= encode32FQDNEncoded
  where
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

    replacePadding :: ByteString -> ByteString
    replacePadding bs = BC.map filterPadding bs
      where
        filterPadding :: Char -> Char
        filterPadding '=' = '9'
        filterPadding c   = toLower c
