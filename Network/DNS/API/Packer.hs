-- |
-- Module      : Network.DNS.API.Packer
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DNS.API.Packer
    ( -- * Types
      DnsPacker
    , runDnsPacker
      -- * default tools
    , putWord8
    , putWord16
    , putWord32
    , putByteString
    , putSizedByteString
    ) where

import Data.ByteString.Pack (Packer)
import qualified Data.ByteString.Pack as BP
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Monoid
import Data.Word
import Foreign.Storable

import Network.DNS.API.Error

data DnsPacker = DnsPacker (Packer ()) Int

instance Monoid DnsPacker where
    mempty = DnsPacker (return ()) 0

    mappend (DnsPacker p1 s1) (DnsPacker p2 s2) = DnsPacker (p1 >> p2) (s1 + s2)

    mconcat [] = mempty
    mconcat a = foldr1 mappend a

-- | help to execute the packing of a bytestring
runDnsPacker :: DnsPacker -> Dns ByteString
runDnsPacker (DnsPacker packer size) =
    let l = BP.pack packer size
    in  case l of
            Left err -> errorDns $ "Network.DNS.API.dnsPack: pack fail: " ++ err ++ " " ++ (show size)
            Right bs -> return bs

putStorable :: Storable storable => storable -> DnsPacker
putStorable s = DnsPacker (BP.putStorable s) (sizeOf s)

putWord8 :: Word8 -> DnsPacker
putWord8 = putStorable

putWord16 :: Word16 -> DnsPacker
putWord16 = putStorable

putWord32 :: Word32 -> DnsPacker
putWord32 = putStorable

putByteString :: ByteString -> DnsPacker
putByteString bs = DnsPacker (BP.putByteString bs) (B.length bs)

putSizedByteString :: ByteString -> DnsPacker
putSizedByteString bs =
       putWord8 (fromIntegral $ B.length bs)
    <> putByteString bs
