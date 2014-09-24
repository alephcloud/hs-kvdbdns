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
module Network.DNS.API.Types
  ( Dns
  , DnsIO
    -- * FQDN encoding
    -- ** Types
  , FQDNEncoded
  , FQDN
    -- ** Accessors
  , encodeFQDN
  , unsafeToFQDN
    -- * Request
    -- ** Class
  , Encodable(..)
  , decodeFQDNEncoded
  , Packable(..)
    -- ** Types
  , Request(..)
    -- * Response
    -- ** Types
  , Response(..)
    -- ** Functions
  , encodeResponse
  , decodeResponse
  ) where

import Control.Applicative
import Control.Monad.Except

import Data.Byteable
import Data.ByteString (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base32 as BSB32
import qualified Data.ByteString.Parse  as BP

import Data.Word (Word8)

type Dns   a = Except String a
type DnsIO a = ExceptT String IO a

------------------------------------------------------------------------------
--                                FQDN                                      --
------------------------------------------------------------------------------

newtype FQDNEncoded = FQDNEncoded ByteString
    deriving (Show, Eq)

instance Byteable FQDNEncoded where
    toBytes (FQDNEncoded bs) = bs

encodeFQDN :: ByteString -> FQDNEncoded
encodeFQDN bs = FQDNEncoded bs

newtype FQDN = FQDN ByteString
    deriving (Show, Eq)

instance Byteable FQDN where
    toBytes (FQDN bs) = bs

unsafeToFQDN :: FQDNEncoded -> FQDN
unsafeToFQDN = FQDN . toBytes

------------------------------------------------------------------------------
--                                   Response                               --
------------------------------------------------------------------------------

-- | The response data that will be return to the requester
data Response p = Response
  { response  :: p
  , signature :: ByteString
  } deriving (Show, Eq)

encodeResponse :: Packable p => Response p -> ByteString
encodeResponse resp = B.concat [sigLength, sig, pack txt]
  where
    sigLength :: ByteString
    sigLength = B.pack [fromIntegral $ B.length sig]
    txt = response  resp
    sig = signature resp

decodeResponse :: Packable p => ByteString -> Dns (Response p)
decodeResponse = dnsParse parser
  where
    parser :: Packable p => BP.Parser (Response p)
    parser = do
        s <- fromIntegral <$> BP.anyByte
        sig <- BP.take s
        res <- unpack
        return $ Response { signature = sig, response = res }

dnsParse :: BP.Parser a -> ByteString -> Dns a
dnsParse parser bs = do
    l <- BP.parseFeed (return B.empty) parser bs
    case l of
        BP.ParseFail err -> throwError $ "Network.DNS.API.Types.dnsParse: parse fail: " ++ err
        BP.ParseMore {}  -> throwError "Network.DNS.API.Types.dnsParse: parse Partial"
        BP.ParseOK b v | B.null b  -> return v
                       | otherwise -> throwError "Network.DNS.API.Types.dnsParse: unparsed data"

------------------------------------------------------------------------------
--                                Encodable                                 --
------------------------------------------------------------------------------

-- | This is the main type to implement to make your requests encodable
--
-- As we use the Domain Name field to send request to the DNS Server we need to
-- encode the URL into a format that will be a valide format for every DNS
-- servers our request may go through.
class Encodable a where
    encode :: a -> Dns FQDNEncoded
    decode :: BP.Parser a

decodeFQDNEncoded :: Encodable a => FQDNEncoded -> Dns a
decodeFQDNEncoded fqdn = decode32FQDNEncoded fqdn >>= dnsParse decode

------------------------------------------------------------------------------
--                              Request                                     --
------------------------------------------------------------------------------

-- | This is the main structure that describes a DNS request
-- Use it to send a DNS query to the DNS-Server
--
-- generate the API byte array:
-- * [1]: nonce length (l >= 0)
-- * [l]: nonce
-- * [1]: command length (s > 0)
-- * [s]:
--     * [1]: the command type
--     * [s-1]: the command params (depends of the command type)
--
-- encode the API byte array into base32 String and append the domain name:
-- > <base32(API byte array)>.<dns domain name>
--
-- And to use it quickly:
-- > encode $ DNSRequest "alephcloud.com." ("hello words!" :: ByteString) "0123456789"
data Request p = Request
  { cmd    :: p          -- ^ the command
  , nonce  :: ByteString -- ^ a nonce to sign the Response
  } deriving (Show, Eq)

instance (Packable p) => Encodable (Request p) where
  encode = encodeRequest
  decode = decodeRequest

-- | This represent a packable
--
-- It is use to pack/unpack (into bytestring) a command in the case of the
-- proposed Request
class Packable p where
  pack   :: p -> ByteString
  unpack :: BP.Parser p

encodeRequest :: Packable p => Request p -> Dns FQDNEncoded
encodeRequest req =
    encode32FQDNEncoded bs
  where
    bs :: ByteString
    bs =
      let nonceBS = nonce req
          cmdBS   = pack $ cmd req
          nonceSize = fromIntegral $ B.length nonceBS :: Word8
      in  B.concat [ B.pack [nonceSize], nonceBS, cmdBS]

decodeRequest :: Packable p
              => BP.Parser (Request p)
decodeRequest = do
    nonceSize <- fromIntegral <$> BP.anyByte
    nonceBS <- BP.take nonceSize
    cmdBS <- unpack
    return $ Request { nonce = nonceBS, cmd = cmdBS }

-- | Encode into a FQDN compatible fornat
encode32FQDNEncoded :: ByteString -> Dns FQDNEncoded
encode32FQDNEncoded bs = encodeFQDN <$> fqdn
  where
    fqdn :: Dns ByteString
    fqdn = either (throwError) (\e -> return $ B.intercalate (B.pack [0x2E]) $ splitByNode e) $ BSB32.encode bs

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
        Left err  -> throwError err
        Right dbs -> return dbs
  where
    bs :: ByteString
    bs = toBytes fqdn
