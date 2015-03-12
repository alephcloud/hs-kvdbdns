-- Copyright (c) 2013-2015 PivotCloud, Inc.
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

{-# LANGUAGE OverloadedStrings #-}

module Network.DNS.API.FQDN
    ( -- * Nodes/FQDN
      Node(..)
    , FQDN(..)
      -- ** Functions
    , appendRootNode
    , getRootNode
    , appendFQDN
    , removeFQDNSuffix
    , removeEmptyNodes
      -- * Unsafe FQDN
    , UnsafeFQDN(..)
      -- * Valid FQDN
    , ValidFQDN
    , validateFQDN
    , validateFQDNEither
    , unsafeValidFQDN
    ) where

import           Data.Byteable
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
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

concatFromNodes_ :: [Node] -> ByteString
concatFromNodes_ = B.intercalate "." . map toBytes . removeEmptyNodes

-------------------------------------------------------------------------------
--                                  FQDN                                     --
-------------------------------------------------------------------------------

-- | All the function to define a FQDN
class Byteable fqdn => FQDN fqdn where
    -- | split the given FQDN into fqdn list of nodes
    --
    -- i.e.: toNodes "example.com" == ["example", "com"]
    -- i.e.: toNodes "example.com." == ["example", "com", ""]
    toNodes :: fqdn -> [Node]

    -- | Create an FQDN from a given list of Nodes
    fromNodes :: [Node] -> Dns fqdn

appendRootNode :: (FQDN fqdn, FQDN output)
               => fqdn
               -> Node
               -> Dns output
appendRootNode fqdn node =
    fromNodes $ (toNodes fqdn) ++ [node]

-- | get the root node
--
-- Fail if the FQDN does not have any nodes
getRootNode :: FQDN fqdn
            => fqdn
            -> Dns Node
getRootNode fqdn =
    getLast $ toNodes fqdn
  where
    getLast :: [Node] -> Dns Node
    getLast []     = errorDns "Network.DNS.API.FQDN.getRootNode: empty node"
    getLast [n]    = return n
    getLast (_:xs) = getLast xs

appendFQDN :: (FQDN fqdn1, FQDN fqdn2, FQDN output)
           => fqdn1
           -> fqdn2
           -> Dns output
appendFQDN main root =
    fromNodes $ (toNodes main) ++ (toNodes root)

-- | remove the given FQDN suffix from the FQDNEncoded
removeFQDNSuffix :: (FQDN main, FQDN suffix, FQDN output)
                 => main
                 -> suffix
                 -> Dns output
removeFQDNSuffix main suffix =
    case makeRemoveSuffix mainNodes suffixNodes of
        Left err -> errorDns $ "Network.DNS.API.FQDN.appendFQDN: " ++ err
        Right l  -> fromNodes $ reverse l
  where
    suffixNodes :: [Node]
    suffixNodes = reverse $ toNodes suffix

    mainNodes :: [Node]
    mainNodes = reverse $ toNodes main

    makeRemoveSuffix :: [Node] -> [Node] -> Either String [Node]
    makeRemoveSuffix a  [] = Right a
    makeRemoveSuffix [] (_:_)  = Left "suffix larger than data"
    makeRemoveSuffix (x:xs) (y:ys)
        | x == y    = makeRemoveSuffix xs ys
        | otherwise = Left "suffix does not match"

-- | remove all the empty nodes but the last
--
-- > removeEmptyNodes [Node "", Node "example", Node "", Node "", Node "com" ] == [Node "example", Node "com" ]
-- > removeEmptyNodes [Node "example", Node "", Node "", Node "com", Node "" ] == [Node "example", Node "com", Node ""]
removeEmptyNodes :: [Node] -> [Node]
removeEmptyNodes l =
    case l of
        []  -> []
        [n] -> [n]
        ((Node n):ns) | (B.null $ toBytes $ n) -> removeEmptyNodes ns
                      | otherwise              -> (Node n):(removeEmptyNodes ns)

instance FQDN ByteString where
    toNodes = splitToNodes_
    fromNodes = return . concatFromNodes_

-------------------------------------------------------------------------------
--                               UnsafeFQDN                                  --
-------------------------------------------------------------------------------

-- | This is an unsafe FQDN
-- Which means it could be an unvalid FQDN
-- see validateFQDN
newtype UnsafeFQDN = UnsafeFQDN ByteString
    deriving (Show, Eq, Ord)
instance Byteable UnsafeFQDN where
    toBytes (UnsafeFQDN bs) = bs

instance FQDN UnsafeFQDN where
    toNodes = splitToNodes_
    fromNodes = return . UnsafeFQDN . concatFromNodes_

-------------------------------------------------------------------------------
--                                ValidFQDN                                  --
-------------------------------------------------------------------------------

-- | represent a valide FQDN
newtype ValidFQDN = ValidFQDN ByteString
    deriving (Show, Eq, Ord)
instance Byteable ValidFQDN where
    toBytes (ValidFQDN bs) = bs

instance FQDN ValidFQDN where
    toNodes = splitToNodes_
    fromNodes = validateFQDN . concatFromNodes_

-- | Unsafe Way to build a ValidFQDN
--
-- You should rather look at the function validateFQDN
-- Which will provides security validations
unsafeValidFQDN :: FQDN fqdn => fqdn -> ValidFQDN
unsafeValidFQDN = ValidFQDN . toBytes

validateFQDNEither :: FQDN unsafe
                   => unsafe
                   -> Either String ValidFQDN
validateFQDNEither = execDns . validateFQDN

-- | Validate a FQDN into a ValidFQDN
--
-- 1. fullLength < 256
-- 2. node's length < 64
-- 3. char are elem of [a-z0-9.-]
validateFQDN :: FQDN unsafe
             => unsafe
             -> Dns ValidFQDN
validateFQDN req = fullLength req >>= nodeLengths >>= checkAlphabet >>= return . unsafeValidFQDN
  where
    fullLength :: FQDN unsafe => unsafe -> Dns unsafe
    fullLength fqdn
        | (B.length $ toBytes fqdn) < 256 = return fqdn
        | otherwise = errorDns "Network.DNS.API.FQDN.validateFQDN: URL too long"
    
    checkAlphabet :: FQDN unsafe => unsafe -> Dns unsafe
    checkAlphabet fqdn
        | B.all checkWord8 $ toBytes fqdn = return fqdn
        | otherwise = errorDns "Network.DNS.API.FQDN.validateFQDN: URL contains non-base32-encoded char"

    checkWord8 :: Char -> Bool
    checkWord8 c
        | c <= 'z' && c >= 'a' = True
        | c <= '9' && c >= '0' = True
        | c == '.' || c == '-' = True
        | otherwise = False

    nodeLengths :: FQDN unsafe => unsafe -> Dns unsafe
    nodeLengths fqdn
        | all checkNodeLength $ toNodes fqdn = return fqdn
        | otherwise = errorDns "Network.DNS.API.FQDN.validateFQDN: URL contains too long labels"

    checkNodeLength :: Node -> Bool
    checkNodeLength n = 64 > (B.length $ toBytes n)
