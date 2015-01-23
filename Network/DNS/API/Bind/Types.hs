-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Network.DNS.API.Bind.Types
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
{-# LANGUAGE RankNTypes #-}
module Network.DNS.API.Bind.Types
    ( BindingFunction(..)
    , BindingA
    , BindingAAAA
    , BindingTXT
    , BindingPTR
    , BindingNS
    , BindingCNAME
    , BindingDNAME
      -- * Option bindings
      -- ** Token and Options
    , Token(..)
    , Opts
    , CommandScope(..)
    , CommandLine(..)
    , getCommandName
      -- ** Error handling
    , TokenError(..)
    , printTokenError
      -- ** CommandLine helpers
    , withCommandLineValues
    , withCommandLineValues'
      -- ** Options helpers
    , toListOpts
    , emptyOpts
    , insertOpts
    , withSafeOpt
    , withUnsafeOpt
    ) where

import           Data.ByteString (ByteString)
import           Data.IP (IPv4, IPv6)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Network.DNS.Types (TYPE(..))
import           Network.DNS.API.Connection
import           Network.DNS.API.Error
import           Network.DNS.API.FQDN
import           Text.Read

-------------------------------------------------------------------------------
--                              Binding                                      --
-------------------------------------------------------------------------------

-- | The function to trigger for a given FQDN+recordType
newtype BindingFunction value = BindingFunction
    { bindingFunction :: forall context . Connection context -> ValidFQDN -> DnsIO value
    }

type BindingA      = BindingFunction [IPv4]
type BindingAAAA   = BindingFunction [IPv6]
type BindingTXT    = BindingFunction [ByteString]
type BindingPTR    = BindingFunction [ValidFQDN]
type BindingNS     = BindingFunction [ValidFQDN]
type BindingCNAME  = BindingFunction [ValidFQDN]
type BindingDNAME  = BindingFunction [ValidFQDN]

-------------------------------------------------------------------------------
--                              Binding options                              --
-------------------------------------------------------------------------------

toListOpts :: Opts -> [(String, Token String)]
toListOpts = Map.toList . getOpts

data Token value = Token
    { tokenLine   :: Int
    , tokenColumn :: Int
    , tokenValue  :: value
    } deriving (Show, Eq, Ord)

data TokenError value = TokenError
    { failedToken :: Token value
    , failedMessage :: String
    } deriving (Show, Eq)

-- | This is a little helper to print a Token Error
-- This could be use for debug/error printing purpose
printTokenError :: Show value => TokenError value -> String
printTokenError te =
    prefixMsg ++ " " ++ (failedMessage te) ++ " in the context of: " ++ (show $ tokenValue token)
  where
    token = failedToken te
    prefixMsg = "line(" ++ (show $ tokenLine token) ++ ") column(" ++ (show $ tokenColumn token) ++ ")"

data CommandScope = CommandScope
    { getCommandScopeName :: Token String
    , getCommandLines     :: [CommandLine]
    , getCommandSubScope  :: [CommandScope]
    } deriving (Show, Eq)

getCommandName :: CommandScope -> String
getCommandName scope = tokenValue $ getCommandScopeName scope

data CommandLine = CommandLine
    { getCommandLineType    :: Token TYPE
    , getCommandLineFQDN    :: Token ValidFQDN
    , getCommandLineOthers  :: [Token String]
    , getCommandLineOptions :: Opts
    } deriving (Show, Eq)

withCommandLineValues :: CommandLine
                      -> ([String] -> a)
                      -> a
withCommandLineValues cmdl f =
    f (map tokenValue $ getCommandLineOthers cmdl)

withCommandLineValues' :: (Read value)
                       => CommandLine
                       -> (Either (TokenError String) [value] -> a)
                       -> a
withCommandLineValues' cmdl f =
    f (mapM tokenReadEither $ getCommandLineOthers cmdl)
  where
    tokenReadEither t =
        case readEither $ tokenValue t of
            Left err -> Left (TokenError t err)
            Right v  -> Right v

newtype Opts = Opts
    { getOpts :: Map String (Token String)
    } deriving (Show, Eq)

insertOpts :: String -> Token String -> Opts -> Opts
insertOpts k v m = Opts $ Map.insert k v (getOpts m)

emptyOpts :: Opts
emptyOpts = Opts Map.empty

withSafeOpt :: Opts -> String -> (Maybe (Token String) -> a) -> a
withSafeOpt opts k f =
    f $ Map.lookup k $ getOpts opts

withUnsafeOpt :: Opts -> String -> (Token String -> a) -> a
withUnsafeOpt opts k f =
    case Map.lookup k $ getOpts opts of
        Nothing -> error $ "Network.DNS.API.Bind: expected options: " ++ k
        Just v  -> f v
