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
  , Request(..)
    -- * Response
  , Response(..)
  , encodeResponse
  , decodeResponse
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base32 as BSB32

import Data.List (intercalate)
import Data.Word (Word8)
import Control.Applicative
import Control.Monad

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

-- | Describe a request sent by client:
-- <param[.param]>.<nonce>.<cmd>.db.net
data Request = Request
  { domain :: ByteString -- ^ the targeted domain (the DN server)
  , cmd    :: ByteString -- ^ the type of request made to the DNS
  , nonce  :: ByteString -- ^ a nonce use by the server to sign the answer
  , param  :: ByteString -- ^ the request param
  } deriving (Show, Eq)

instance Encodable Request where
  encode req =
    (B.intercalate (B.pack [0x2E])) <$> sequence [eparam, enonce, ecmd, edom]
    where
      edom   = pure   $ domain req
      ecmd   = encode $ cmd req
      enonce = encode $ nonce req
      eparam = encode $ param req
  decode dom bs =
    Request
      <$> pure dom
      <*> decode dom rCmd
      <*> decode dom rNonce
      <*> decode dom rParam
    where
      paramNonceCmd = B.take (B.length bs - B.length dom - 1) bs
      (paramNonceDot, rCmd) = B.spanEnd (/= 0x2E) paramNonceCmd
      paramNonce = B.take (B.length paramNonceDot - 1) paramNonceDot
      (paramDot, rNonce) = B.spanEnd (/= 0x2E) paramNonce
      (rParam, _) = B.spanEnd (== 0x0E) paramDot

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
    splitByNode bs
      | B.length bs < 63 = [bs]
      | otherwise = node:(splitByNode xs)
      where
        (node, xs) = B.splitAt 63 bs

-- Decode an URL:
-- Split the bytestring into node (split at every '.')
-- and then decode the result
decodeURL :: (Monad m, Applicative m) => ByteString -> m ByteString
decodeURL bs = BSB32.decode fusion
  where
    fusion :: ByteString
    fusion = B.concat $ B.split 0x2E bs
