-- |
-- Module      : Network.DNS.API.Bind
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@pivotmail.com>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DNS.API.Bind
    ( -- * Main function
      DNSBindings(..)
    , emptyDNSBindings
    , insertDNSBindings

    , DefaultBinding(..)

      -- * Bind Configuration
    , BindingLine
    , parseBindFile

      -- * Write your own extension
      -- ** Class
    , Binding(..)
    , BindingFunction(..)
    , BindingA
    , BindingAAAA
    , BindingTXT
    , BindingPTR
    , BindingNS
    , BindingCNAME
    , BindingDNAME
      -- *** The default error message
    , notImplementedBinding

      -- ** Options
    , Opts
    , emptyOpts
    , insertOpts
    , withSafeOpt
    , withUnsafeOpt

      -- ** Collections
    , Bindings
    , BindingsA
    , BindingsAAAA
    , BindingsTXT
    , BindingsPTR
    , BindingsNS
    , BindingsCNAME
    , BindingsDNAME
    , emptyBindings
    , insertBinding
    , findBinding
    ) where

import           Data.ByteString (ByteString)
import           Data.IP (IPv4, IPv6)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Network.DNS (TYPE(..))
import           Network.DNS.API.FQDN
import           Network.DNS.API.Error

import           Network.DNS.API.Bind.Class
import           Network.DNS.API.Bind.Types
import           Network.DNS.API.Bind.Parser

-------------------------------------------------------------------------------
--                              Bindings                                     --
-------------------------------------------------------------------------------

-- | The collection of bindings for a given instance
data DNSBindings = DNSBindings
    { bindingsA     :: BindingsA
    , bindingsAAAA  :: BindingsAAAA
    , bindingsTXT   :: BindingsTXT
    , bindingsPTR   :: BindingsPTR
    , bindingsNS    :: BindingsNS
    , bindingsCNAME :: BindingsCNAME
    , bindingsDNAME :: BindingsDNAME
    } deriving (Show)

emptyDNSBindings :: DNSBindings
emptyDNSBindings = DNSBindings
    { bindingsA     = emptyBindings
    , bindingsAAAA  = emptyBindings
    , bindingsTXT   = emptyBindings
    , bindingsPTR   = emptyBindings
    , bindingsNS    = emptyBindings
    , bindingsCNAME = emptyBindings
    , bindingsDNAME = emptyBindings
    }

-- | This is the function to use to create a the DNSBindings object
--
-- This function will insert every BindingLine in the given DNSBindings
-- and will return the same tuple but the list will be return without the added
-- Binding in the DNSBindings
--
-- example:
-- > list <- parseBindFile "bind.conf"
-- > let (l', bindings) = insertDNSBindings MyCustomBinding
-- >                    $ insertDNSBindings DefaultBinding
-- >                    $ (list, emptyDNSBindings)
-- > when (not . null l') $ error $ "the given bindings haren't assigned: " ++ show l'
insertDNSBindings :: Binding binding => binding -> ([BindingLine], DNSBindings) -> ([BindingLine], DNSBindings)
insertDNSBindings binding (l, db) = foldr insertB ([], db) l
  where
    name :: String
    name = getName binding

    insertB :: BindingLine -> ([BindingLine], DNSBindings) -> ([BindingLine], DNSBindings)
    insertB b (accList, accDB)
        | name /= (getLineCommand b) = (b:accList, accDB)
        | otherwise                  = (accList, insertDNSDBFilter binding b accDB)

insertDNSDBFilter :: Binding binding => binding -> BindingLine -> DNSBindings -> DNSBindings
insertDNSDBFilter binding bl dnsbs =
    case getLineType bl of
        A     -> insertDNSBindingA binding bl dnsbs
        AAAA  -> insertDNSBindingAAAA binding bl dnsbs
        TXT   -> insertDNSBindingTXT binding bl dnsbs
        PTR   -> insertDNSBindingPTR binding bl dnsbs
        NS    -> insertDNSBindingNS binding bl dnsbs
        CNAME -> insertDNSBindingCNAME binding bl dnsbs 
        DNAME -> insertDNSBindingDNAME binding bl dnsbs 
        t     -> error $ "Type: " ++ show t ++ " not supported yet"

insertDNSBindingA :: Binding binding => binding -> BindingLine -> DNSBindings -> DNSBindings
insertDNSBindingA binding bl dnsbs =
    case execDns $ getA binding (getLineOptions bl) of
        Left err   -> error $ (show bl) ++ ": reported error: " ++ err
        Right func -> dnsbs { bindingsA = insertBinding (getLineFQDN bl) func (bindingsA dnsbs) }

insertDNSBindingAAAA :: Binding binding => binding -> BindingLine -> DNSBindings -> DNSBindings
insertDNSBindingAAAA binding bl dnsbs =
    case execDns $ getAAAA binding (getLineOptions bl) of
        Left err   -> error $ (show bl) ++ ": reported error: " ++ err
        Right func -> dnsbs { bindingsAAAA = insertBinding (getLineFQDN bl) func (bindingsAAAA dnsbs) }

insertDNSBindingTXT :: Binding binding => binding -> BindingLine -> DNSBindings -> DNSBindings
insertDNSBindingTXT binding bl dnsbs =
    case execDns $ getTXT binding (getLineOptions bl) of
        Left err   -> error $ (show bl) ++ ": reported error: " ++ err
        Right func -> dnsbs { bindingsTXT = insertBinding (getLineFQDN bl) func (bindingsTXT dnsbs) }

insertDNSBindingPTR :: Binding binding => binding -> BindingLine -> DNSBindings -> DNSBindings
insertDNSBindingPTR binding bl dnsbs =
    case execDns $ getPTR binding (getLineOptions bl) of
        Left err   -> error $ (show bl) ++ ": reported error: " ++ err
        Right func -> dnsbs { bindingsPTR = insertBinding (getLineFQDN bl) func (bindingsPTR dnsbs) }

insertDNSBindingNS :: Binding binding => binding -> BindingLine -> DNSBindings -> DNSBindings
insertDNSBindingNS binding bl dnsbs =
    case execDns $ getNS binding (getLineOptions bl) of
        Left err   -> error $ (show bl) ++ ": reported error: " ++ err
        Right func -> dnsbs { bindingsNS = insertBinding (getLineFQDN bl) func (bindingsNS dnsbs) }

insertDNSBindingCNAME :: Binding binding => binding -> BindingLine -> DNSBindings -> DNSBindings
insertDNSBindingCNAME binding bl dnsbs =
    case execDns $ getCNAME binding (getLineOptions bl) of
        Left err   -> error $ (show bl) ++ ": reported error: " ++ err
        Right func -> dnsbs { bindingsCNAME = insertBinding (getLineFQDN bl) func (bindingsCNAME dnsbs) }

insertDNSBindingDNAME :: Binding binding => binding -> BindingLine -> DNSBindings -> DNSBindings
insertDNSBindingDNAME binding bl dnsbs =
    case execDns $ getDNAME binding (getLineOptions bl) of
        Left err   -> error $ (show bl) ++ ": reported error: " ++ err
        Right func -> dnsbs { bindingsDNAME = insertBinding (getLineFQDN bl) func (bindingsDNAME dnsbs) }

-------------------------------------------------------------------------------
--                              Generic bindings                             --
-------------------------------------------------------------------------------

data BindingNode value = BindingNode
    { getFunc     :: Maybe (BindingFunction value)
    , getBindings :: Bindings value
    }
instance Show (BindingNode a) where
    show node = "BindingNode { getFunc = " ++ action ++ ", getBindings = " ++ show (getBindings node) ++ " }"
      where
        action :: String
        action = maybe "Nothing" (\_ -> "Just <action>") $ getFunc node
type Bindings value = Map Node (BindingNode value)

type BindingsA      = Bindings [IPv4]
type BindingsAAAA   = Bindings [IPv6]
type BindingsTXT    = Bindings [ByteString]
type BindingsPTR    = Bindings [ValidFQDN]
type BindingsNS     = Bindings [ValidFQDN]
type BindingsCNAME  = Bindings [ValidFQDN]
type BindingsDNAME  = Bindings [ValidFQDN]


emptyBindings :: Bindings value
emptyBindings = Map.empty

findNodeBindings :: Node -> Bindings a -> Maybe (BindingNode a)
findNodeBindings = Map.lookup

-- | Return the action associated to the given FQDN
findBinding :: FQDN fqdn => fqdn -> Bindings a -> Dns (BindingFunction a, fqdn)
findBinding fqdn bindings =
    case recursiveFind (reverse $ splitToNodes fqdn) bindings of
        Nothing      -> errorDns "Not found"
        Just (a, ln) -> do
            arg <- fromNodes $ reverse ln
            return $ (a, arg)
  where
    recursiveFind :: [Node] -> Bindings a -> Maybe (BindingFunction a, [Node])
    recursiveFind []     _ = Nothing
    recursiveFind [x]    b =
        case findNodeBindings x b of
            Nothing -> Nothing
            Just sb -> maybe Nothing (\t -> Just (t, [])) $ getFunc sb
    recursiveFind (x:xs) b =
        case findNodeBindings x b of
            Nothing -> Nothing
            Just sb ->
                case recursiveFind xs (getBindings sb) of
                    Just qq -> Just qq
                    Nothing -> maybe Nothing (\t -> Just (t, xs)) $ getFunc sb

-- | Bind an action with to given FQDN
insertBinding :: FQDN fqdn
              => fqdn
              -> BindingFunction a
              -> Bindings a
              -> Bindings a
insertBinding fqdn action bindings = insertNodes action (reverse $ splitToNodes fqdn) bindings
  where
    insertNodes :: BindingFunction a -> [Node] -> Bindings a -> Bindings a
    insertNodes _ []     _ = error "the FQDN might be null and it is non-sense to add a binding for nothing"
    -- Insert the node in the given Bindings collection
    -- If the node alread exist in it will be erased
    insertNodes a [x]    b = insertNodeBindings x (BindingNode (Just a) emptyBindings) b
    insertNodes a (x:xs) b =
        case findNodeBindings x b of
            Nothing ->
                let newSubb = BindingNode Nothing (insertNodes a xs emptyBindings)
                in  insertNodeBindings x newSubb b
            Just subb ->
                let newSubb = subb { getBindings = insertNodes a xs (getBindings subb) }
                in  insertNodeBindings x newSubb b

    insertNodeBindings :: Node -> BindingNode a -> Bindings a -> Bindings a
    insertNodeBindings = Map.insert
