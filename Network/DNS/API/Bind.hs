-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Network.DNS.API.Bind
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

module Network.DNS.API.Bind
    ( -- * Main function
      DNSBindings(..)
    , emptyDNSBindings
    , insertDNSBindings

    , DefaultBinding(..)

      -- * Bind Configuration
    , CommandScope(..)
    , CommandLine(..)
    , Token(..)
    , TokenError(..)
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
    , notSupportedBinding
    , commandParsingError

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
import           Data.List
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

-- | This is the function wich will lookup given Binding in the configuration file
-- if the Binding has been enable and configured properly
--
-- If the binding has no scope, then we suppose this is a Default Binding for this
-- application and we parse the Binding lines
insertDNSBindings :: Binding binding
                  => binding -- ^ The binding to add
                  -> Dns (CommandScope, DNSBindings) -- ^ The scope and the binding to add
                  -> Dns (CommandScope, DNSBindings) -- ^ The scope and the updated binding
insertDNSBindings binding getInitialState = do
    (scope, db) <- getInitialState
    case scopeList of
        -- In the case we are parsing the default Commands
        [] -> do
            db' <- insertDNSBindingsInScope binding scope db
            return (scope, db')
        -- Else, we go to insert the bindings in a appropriate scope
        _  -> do
            db' <- insertDNSBindingsRecursiveScope scopeList binding scope db
            return (scope, db')
  where
    scopeList :: [String]
    scopeList = getName binding

insertDNSBindingsRecursiveScope :: Binding binding
                                => [String]
                                -> binding
                                -> CommandScope
                                -> DNSBindings
                                -> Dns DNSBindings
insertDNSBindingsRecursiveScope scopeList binding scope db =
    case scopeList of
        -- This means the Binding hasn't been configured in the Binding File
        []     -> return db
        -- Look up for the exact scope
        [x]    ->
            case find (\n -> x == getCommandName n) $ getCommandSubScope scope of
                -- This means the Binding file hasn't been configured in the scope
                Nothing -> return db
                -- This means the scope is present and we need to insert it in
                Just v  -> do
                    -- update the binding data base
                    db' <- insertDNSBindingsInScope binding v db
                    -- return the binding data base
                    return db'
        -- In this case we need to lookup into the command scope to get the actual present binding
        (x:xs) ->
            case find (\n -> x == getCommandName n) $ getCommandSubScope scope of
                -- This means the Binding file hasn't been configured in the scope
                Nothing -> return db
                -- Look up in the sub scope of the found command scope
                Just v  -> insertDNSBindingsRecursiveScope xs binding v db

-- | At this stage we already trust the given binding is configured in this
-- scope. This stage will get all the given bindings and insert them in the
-- DNSBindings.
insertDNSBindingsInScope :: Binding binding
                         => binding
                         -> CommandScope
                         -> DNSBindings
                         -> Dns DNSBindings
insertDNSBindingsInScope binding scope db =
    insertDNSBindingsCommandLines binding (getCommandLines scope) db

insertDNSBindingsCommandLines :: Binding binding
                              => binding
                              -> [CommandLine]
                              -> DNSBindings
                              -> Dns DNSBindings
insertDNSBindingsCommandLines _       []     db = return db
insertDNSBindingsCommandLines binding (x:xs) db = do
    db' <- insertDNSDBFilter binding x db
    insertDNSBindingsCommandLines binding xs db'

insertDNSDBFilter :: Binding binding => binding -> CommandLine -> DNSBindings -> Dns DNSBindings
insertDNSDBFilter binding cl dnsbs =
    case getLineType of
        A     -> insertDNSBindingA     binding cl dnsbs
        AAAA  -> insertDNSBindingAAAA  binding cl dnsbs
        TXT   -> insertDNSBindingTXT   binding cl dnsbs
        PTR   -> insertDNSBindingPTR   binding cl dnsbs
        NS    -> insertDNSBindingNS    binding cl dnsbs
        CNAME -> insertDNSBindingCNAME binding cl dnsbs
        DNAME -> insertDNSBindingDNAME binding cl dnsbs
        _     -> errorDns $ printTokenError $ TokenError linetype "This type is not yet supported by the system"
  where
    getLineType :: TYPE
    getLineType = tokenValue linetype
    linetype :: Token TYPE
    linetype = getCommandLineType cl

insertDNSBindingA :: Binding binding => binding -> CommandLine -> DNSBindings -> Dns DNSBindings
insertDNSBindingA binding cl dnsbs = do
    f <- getA binding cl
    return $ dnsbs { bindingsA = insertBinding (tokenValue $ getCommandLineFQDN cl) f (bindingsA dnsbs) }

insertDNSBindingAAAA :: Binding binding => binding -> CommandLine -> DNSBindings -> Dns DNSBindings
insertDNSBindingAAAA binding cl dnsbs = do
    f <- getAAAA binding cl
    return $ dnsbs { bindingsAAAA = insertBinding (tokenValue $ getCommandLineFQDN cl) f (bindingsAAAA dnsbs) }

insertDNSBindingTXT :: Binding binding => binding -> CommandLine -> DNSBindings -> Dns DNSBindings
insertDNSBindingTXT binding cl dnsbs = do
    f <- getTXT binding cl
    return $ dnsbs { bindingsTXT = insertBinding (tokenValue $ getCommandLineFQDN cl) f (bindingsTXT dnsbs) }

insertDNSBindingPTR :: Binding binding => binding -> CommandLine -> DNSBindings -> Dns DNSBindings
insertDNSBindingPTR binding cl dnsbs = do
    f <- getPTR binding cl
    return $ dnsbs { bindingsPTR = insertBinding (tokenValue $ getCommandLineFQDN cl) f (bindingsPTR dnsbs) }

insertDNSBindingNS :: Binding binding => binding -> CommandLine -> DNSBindings -> Dns DNSBindings
insertDNSBindingNS binding cl dnsbs = do
    f <- getNS binding cl
    return $ dnsbs { bindingsNS = insertBinding (tokenValue $ getCommandLineFQDN cl) f (bindingsNS dnsbs) }

insertDNSBindingCNAME :: Binding binding => binding -> CommandLine -> DNSBindings -> Dns DNSBindings
insertDNSBindingCNAME binding cl dnsbs = do
    f <- getCNAME binding cl
    return $ dnsbs { bindingsCNAME = insertBinding (tokenValue $ getCommandLineFQDN cl) f (bindingsCNAME dnsbs) }

insertDNSBindingDNAME :: Binding binding => binding -> CommandLine -> DNSBindings -> Dns DNSBindings
insertDNSBindingDNAME binding cl dnsbs = do
    f <- getDNAME binding cl
    return $ dnsbs { bindingsDNAME = insertBinding (tokenValue $ getCommandLineFQDN cl) f (bindingsDNAME dnsbs) }

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
    case recursiveFind (reverse $ toNodes fqdn) bindings of
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
insertBinding fqdn action bindings = insertNodes action (reverse $ toNodes fqdn) bindings
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
