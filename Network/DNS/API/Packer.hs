-- Copyright (c) 2013-2015 PivotCloud, Inc.
--
-- Network.DNS.API.Packer
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

module Network.DNS.API.Packer
    ( -- * Types
      DnsPacker
    , dnsPack
    , getPackedSize
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

data DnsPacker = DnsPacker
    { getPacker     :: Packer ()
    , getNeededSize :: Int
    }

instance Monoid DnsPacker where
    mempty = DnsPacker (return ()) 0

    mappend (DnsPacker p1 s1) (DnsPacker p2 s2) = DnsPacker (p1 >> p2) (s1 + s2)

    mconcat [] = mempty
    mconcat a = foldr1 mappend a

getPackedSize :: DnsPacker -> Int
getPackedSize (DnsPacker _ size) = size

-- | Run a DNS Packer and return the packed string
dnsPack :: DnsPacker
        -> Dns ByteString
dnsPack packer =
    let l = BP.pack (getPacker packer) (getNeededSize packer)
    in  case l of
            Left err -> errorDns $ "Network.DNS.API.dnsPack: pack fail: " ++ err ++ " " ++ (show $ getNeededSize packer)
            Right bs -> return bs

putStorable :: Storable storable
            => storable
            -> DnsPacker
putStorable s = DnsPacker (BP.putStorable s) (sizeOf s)

putWord8 :: Word8 -> DnsPacker
putWord8 = putStorable

putWord16 :: Word16 -> DnsPacker
putWord16 = putStorable

putWord32 :: Word32 -> DnsPacker
putWord32 = putStorable

putByteString :: ByteString
              -> DnsPacker
putByteString bs = DnsPacker (BP.putByteString bs) (B.length bs)

-- | Equivalent to putByteString but it first keep pack the size into
-- the first Bytes (the length of the string can't be upper than 255)
putSizedByteString :: ByteString
                   -> DnsPacker
putSizedByteString bs =
       putWord8 (fromIntegral $ B.length bs)
    <> putByteString bs
