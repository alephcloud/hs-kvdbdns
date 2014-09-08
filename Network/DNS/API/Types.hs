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
  ( -- * Request
    Encodable(..)
  , Packable(..)
  , Request(..)
    -- * Response
  , Response(..)
  , encodeResponse
  , decodeResponse
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base32 as BSB32

import Data.Word (Word8)
import Control.Applicative

------------------------------------------------------------------------------
--                                   Response                               --
------------------------------------------------------------------------------

-- | The response data that will be return to the requester
data Response = Response
  { response  :: ByteString
  , signature :: ByteString
  } deriving (Show, Eq)

encodeResponse :: Response -> ByteString
encodeResponse resp = B.concat [sigLength, sig, txt]
  where
    sigLength :: ByteString
    sigLength = B.pack [fromIntegral $ B.length sig]
    txt = response  resp
    sig = signature resp

decodeResponse :: ByteString -> Response
decodeResponse bs =
  Response
    { response  = txt
    , signature = sig
    }
  where
    sigLength :: Int
    sigLength = fromIntegral $ B.head bs
    sig = B.take sigLength $ B.drop 1 bs
    txt = B.drop (1 + sigLength) bs

------------------------------------------------------------------------------
--                                 Request                                  --
------------------------------------------------------------------------------

-- | This is the main type to implement to make your requests encodable
--
-- As we use the Domain Name field to send request to the DNS Server we need to
-- encode the URL into a format that will be a valide format for every DNS
-- servers our request may go through.
class Encodable a where
  encode :: (Monad m, Applicative m) => a -> m ByteString
  decode :: (Monad m, Applicative m) => ByteString -> ByteString -> m a

------------------------------------------------------------------------------
--                          Encodable request                               --
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

-- | This represent a packable
--
-- It is use to pack/unpack (into bytestring) a command in the case of the
-- proposed Request
class Packable p where
  pack :: p -> ByteString
  unpack :: (Monad m, Applicative m) => ByteString -> m p

instance Packable ByteString where
  pack = id
  unpack = pure.id

instance (Packable p) => Encodable (Request p) where
  encode req = B.concat <$> sequence [encoded, pure $ B.pack [0x2E], pure $ domain req]
    where
      encoded :: (Monad m, Applicative m) => m ByteString
      encoded =
        let nonceBS = nonce req
            cmdBS   = pack $ cmd req
            nonceSize = fromIntegral $ B.length nonceBS :: Word8
            cmdSize   = fromIntegral $ B.length cmdBS :: Word8
        in  encode $ B.concat [ B.pack [nonceSize]
                              , nonceBS
                              , B.pack [cmdSize]
                              , cmdBS
                              ]

  decode dom bs =
      Request
        <$> pure dom
        <*> (unpack =<< command)
        <*> ((\l s -> (B.take l $ B.drop 1 s)) <$> nonceSize <*> decoded)
    where
      decoded :: (Monad m, Applicative m) => m ByteString
      decoded = decode dom $ B.take (B.length bs - B.length dom - 1) bs

      nonceSize :: (Monad m, Applicative m) => m Int
      nonceSize = (fromIntegral . B.head) <$> decoded

      commandAndSize :: (Monad m, Applicative m) => m ByteString
      commandAndSize = (\l s -> B.drop (l + 1) s) <$> nonceSize <*> decoded
      command :: (Monad m, Applicative m) => m ByteString
      command = (B.drop 1) <$> commandAndSize

instance Encodable ByteString where
  encode = encodeURL
  decode _ = decodeURL

-- Encode a bytestring and split it in node of size 63 (or less)
-- then intercalate the node separator '.'
encodeURL :: (Monad m, Applicative m)
          => ByteString -> m ByteString
encodeURL bs
  | guessedLength > 200 = fail "bytestring too long"
  | otherwise = (B.intercalate (B.pack [0x2E])) <$> splitByNode <$> e
  where
    e :: (Monad m, Applicative m) => m ByteString
    e = BSB32.encode bs
    guessedLength :: Int
    guessedLength = BSB32.guessEncodedLength $ B.length bs

    splitByNode :: ByteString -> [ByteString]
    splitByNode b
      | (B.length b) < 63 = [b]
      | otherwise = node:(splitByNode xs)
      where
        (node, xs) = B.splitAt 63 b

-- Decode an URL:
-- Split the bytestring into node (split at every '.')
-- and then decode the result
decodeURL :: (Monad m, Applicative m) => ByteString -> m ByteString
decodeURL bs = BSB32.decode fusion
  where
    fusion :: ByteString
    fusion = B.concat $ B.split 0x2E bs
