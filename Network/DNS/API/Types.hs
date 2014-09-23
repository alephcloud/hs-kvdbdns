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

import Data.Byteable
import Data.ByteString (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Base32 as BSB32

import Data.Word (Word8)
import Control.Applicative
import Control.Monad.Except

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
decodeResponse bs =
  unpack txt >>= \t -> return $ Response { response  = t, signature = sig }
  where
    sigLength :: Int
    sigLength = fromIntegral $ B.head bs
    sig = B.take sigLength $ B.drop 1 bs
    txt = B.drop (1 + sigLength) bs

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
  decode :: ByteString -> FQDNEncoded -> Dns a

instance Encodable ByteString where
  encode   = encodeByteString
  decode _ = decodeByteString

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
  { domain :: ByteString -- ^ the DNS-Server Domain Name
  , cmd    :: p          -- ^ the command
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
  unpack :: ByteString -> Dns p

instance Packable ByteString where
  pack   = id
  unpack = pure.id

instance Packable String where
  pack   = BC.pack
  unpack = pure . BC.unpack

encodeRequest :: Packable p => Request p -> Dns FQDNEncoded
encodeRequest req =
    return.encodeFQDN =<< (B.concat <$> sequence [encoded, pure $ B.pack [0x2E], pure $ domain req])
  where
    encoded :: Dns ByteString
    encoded =
      let nonceBS = nonce req
          cmdBS   = pack $ cmd req
          nonceSize = fromIntegral $ B.length nonceBS :: Word8
          cmdSize   = fromIntegral $ B.length cmdBS :: Word8
      in  toBytes <$> (encodeByteString $ B.concat [ B.pack [nonceSize]
                                                   , nonceBS
                                                   , B.pack [cmdSize]
                                                   , cmdBS
                                                   ])

decodeRequest :: Packable p
              => ByteString
              -> FQDNEncoded
              -> Dns (Request p)
decodeRequest dom fqdn =
    Request
      <$> pure dom
      <*> (unpack =<< command)
      <*> ((\l s -> (B.take l $ B.drop 1 s)) <$> nonceSize <*> decoded)
  where
    bs :: ByteString
    bs = toBytes fqdn

    decoded :: Dns ByteString
    decoded = decode dom $ FQDNEncoded $ B.take (B.length bs - B.length dom - 1) bs

    nonceSize :: Dns Int
    nonceSize = (fromIntegral . B.head) <$> decoded

    commandAndSize :: Dns ByteString
    commandAndSize = (\l s -> B.drop (l + 1) s) <$> nonceSize <*> decoded
    command :: Dns ByteString
    command = (B.drop 1) <$> commandAndSize

-- | Encode into a FQDN compatible fornat
encodeByteString :: ByteString -> Dns FQDNEncoded
encodeByteString bs = return.encodeFQDN =<< (B.intercalate (B.pack [0x2E])) <$> splitByNode <$> e
  where
    e :: Dns ByteString
    e = either (throwError) (return) $ BSB32.encode bs

    splitByNode :: ByteString -> [ByteString]
    splitByNode b
      | (B.length b) < 63 = [b]
      | otherwise = node:(splitByNode xs)
      where
        (node, xs) = B.splitAt 63 b

-- Decode an URL:
-- Split the bytestring into nodes (split at every '.')
-- and then concat and decode the result
decodeByteString :: FQDNEncoded -> Dns ByteString
decodeByteString fqdn =
  case BSB32.decode $ B.concat $ B.split 0x2E $ toBytes fqdn of
    Left err  -> throwError err
    Right dbs -> return dbs
