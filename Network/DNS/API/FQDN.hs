-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Network.DNS.API.FQDN
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
{-# LANGUAGE OverloadedStrings #-}
module Network.DNS.API.FQDN
    ( Node(..)
    , FQDN(..)
    , removeFQDNSuffix
      -- * Unsafe FQDN
    , UnsafeFQDN(..)
      -- * Valid FQDN
    , ValidFQDN
    , validateFQDN
    , unsafeValidFQDN
    ) where

import           Control.Applicative
import           Data.Byteable
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe (isJust)
import           Network.DNS.API.Error

-------------------------------------------------------------------------------
--                                  Node                                     --
-------------------------------------------------------------------------------

newtype Node = Node ByteString
    deriving (Show, Eq, Ord)
instance Byteable Node where
    toBytes (Node bs) = bs

-------------------------------------------------------------------------------
--                                  Common                                   --
-------------------------------------------------------------------------------

splitToNodes_ :: Byteable a => a -> [Node]
splitToNodes_ = map Node . B.split '.' . toBytes

popRootNode_ :: Byteable a => a -> (ByteString, Node)
popRootNode_ bs =
    let (bs', n) = B.spanEnd ((/=) '.') $ toBytes bs
    in  (bs', Node n)

appendFQDN_ :: Byteable a
            => a
            -> Node
            -> ByteString
appendFQDN_ main root = B.intercalate "." [toBytes main, toBytes root]

-------------------------------------------------------------------------------
--                                  FQDN                                     --
-------------------------------------------------------------------------------

class Byteable a => FQDN a where
    getRootNode  :: a -> (a, Node)
    splitToNodes :: a -> [Node]
    appendNode   :: a -> Node -> Dns a
    fromNodes    :: [Node] -> Dns a
    removeSuffix :: a -> a -> Dns a
    appendFQDN   :: FQDN b => a -> b -> Dns a

    -- | default implementation is a combination of:
    -- removeFQDNSuffix (the function)
    -- fromNodes
    removeSuffix main suffix = fromNodes =<< removeFQDNSuffix main suffix

    -- | default implementation is a combination of:
    -- splitToNodes
    -- fromNodes
    appendFQDN main suffix = fromNodes $ splitToNodes main ++ splitToNodes suffix

instance FQDN ByteString where
    getRootNode = popRootNode_
    splitToNodes = splitToNodes_
    appendNode fqdn node = return $ appendFQDN_ fqdn node
    fromNodes = return . B.intercalate "." . map toBytes

-- | remove the given FQDN suffix from the FQDNEncoded
removeFQDNSuffix :: (FQDN main, FQDN suffix)
                 => main
                 -> suffix
                 -> Dns [Node]
removeFQDNSuffix encoded dom =
    case makeRemoveSuffix encNodes domNodes of
        Left err -> errorDns err
        Right l  -> return $ reverse l
  where
    domNodes :: [Node]
    domNodes = reverse $ splitToNodes dom

    encNodes :: [Node]
    encNodes = reverse $ splitToNodes encoded

    makeRemoveSuffix :: [Node] -> [Node] -> Either String [Node]
    makeRemoveSuffix a  [] = Right a
    makeRemoveSuffix [] (_:_)  = Left "suffix larger than data"
    makeRemoveSuffix (x:xs) (y:ys)
        | x == y    = makeRemoveSuffix xs ys
        | otherwise = Left "suffix does not match"

-------------------------------------------------------------------------------
--                                ValidFQDN                                  --
-------------------------------------------------------------------------------

-- | represent a valide FQDN
newtype ValidFQDN = ValidFQDN ByteString
    deriving (Show, Eq, Ord)
instance Byteable ValidFQDN where
    toBytes (ValidFQDN bs) = bs

instance FQDN ValidFQDN where
    getRootNode f =
        let (bs, n) = popRootNode_ f
        in  (ValidFQDN bs, n)
    splitToNodes = splitToNodes_
    appendNode fqdn node = validateFQDN $ appendFQDN_ fqdn node
    fromNodes l = validateFQDN =<< (fromNodes l :: Dns ByteString)

-------------------------------------------------------------------------------
--                               UnsafeFQDN                                  --
-------------------------------------------------------------------------------

-- | represent a encoded but not validated FQDN
-- (means that this FQDN is base32 encoded and but may not be a valide FQDN
newtype UnsafeFQDN = UnsafeFQDN ByteString
    deriving (Show, Eq, Ord)
instance Byteable UnsafeFQDN where
    toBytes (UnsafeFQDN bs) = bs

instance FQDN UnsafeFQDN where
    getRootNode f =
        let (bs, n) = popRootNode_ f
        in  (UnsafeFQDN bs, n)
    splitToNodes = splitToNodes_
    appendNode fqdn node = return $ UnsafeFQDN $ appendFQDN_ fqdn node
    fromNodes l = UnsafeFQDN <$> (fromNodes l :: Dns ByteString)

-- | Unsafe Way to build a ValidFQDN
--
-- You should rather look at the function validateFQDN
-- Which will provides security validations
unsafeValidFQDN :: (FQDN a, Byteable a) => a -> ValidFQDN
unsafeValidFQDN = ValidFQDN . toBytes

-- | Check the FQDNEncoded is a valid FQDN and if it is, it returns the FQDN
--
-- 1. fullLength < 256
-- 2. node's length < 64
-- 3. char are elem of [a-z0-9.-]
validateFQDN :: (Byteable unsafe, FQDN unsafe)
             => unsafe
             -> Dns ValidFQDN
validateFQDN req = fullLength req >>= nodeLengths >>= checkAlphabet >>= return . unsafeValidFQDN
  where
    fullLength :: (Byteable unsafe, FQDN unsafe) => unsafe -> Dns unsafe
    fullLength fqdn
        | (B.length $ toBytes fqdn) < 256 = return fqdn
        | otherwise = errorDns "Network.DNS.API.Utils: checkEncoding: URL too long"
    
    checkAlphabet :: (Byteable unsafe, FQDN unsafe) => unsafe -> Dns unsafe
    checkAlphabet fqdn
        | B.foldr checkWord8 True $ toBytes fqdn = return fqdn
        | otherwise = errorDns "Network.DNS.API.Utils: checkEncoding: URL contains non-base32-encoded char"

    checkWord8 :: Char -> Bool -> Bool
    checkWord8 _ False = False
    checkWord8 c True
        | c <= 'z' && c >= 'a' = True
        | c <= '9' && c >= '0' = True
        | c == '.' || c == '-' = True
        | otherwise = False

    nodeLengths :: (Byteable unsafe, FQDN unsafe) => unsafe -> Dns unsafe
    nodeLengths fqdn
        | isJust $ B.foldr checkNodeW (Just 0) $ toBytes fqdn = return fqdn
        | otherwise = errorDns "Network.DNS.API.Utils: checkEncoding: URL contains too long labels"

    checkNodeW :: Char -> Maybe Int -> Maybe Int
    checkNodeW _ Nothing  = Nothing
    checkNodeW w (Just c)
        | c > 64    = Nothing
        | w == '.'  = Just 0
        | otherwise = Just $ c + 1

