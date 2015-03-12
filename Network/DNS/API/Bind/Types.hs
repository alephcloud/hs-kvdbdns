-- Copyright (c) 2013-2015 PivotCloud, Inc.
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

{-# LANGUAGE RankNTypes #-}

module Network.DNS.API.Bind.Types
    ( -- * BindingFunction

      -- ** Definition

      BindingFunction(..)

      -- ** Alias
    , BindingA
    , BindingAAAA
    , BindingTXT
    , BindingPTR
    , BindingNS
    , BindingCNAME
    , BindingDNAME
    , BindingMX
    , BindingSOA
    , BindingSRV

      -- * Binding Command

      -- ** Token

    , Token(..)
    , TokenError(..)
    , printTokenError
    , commandParsingError

      -- ** Command

    , CommandScope(..)
    , CommandLine(..)
    , getCommandName

      -- ** CommandLine helpers

    , withCommandLineFlags
    , withCommandLineFlags'

      -- ** Options

    , Opts
    , toListOpts
    , emptyOpts
    , insertOpts
    , withOpt
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
    { bindingFunction :: Connection -> ValidFQDN -> DnsIO value
    }

type BindingA      = BindingFunction [IPv4]
type BindingAAAA   = BindingFunction [IPv6]
type BindingTXT    = BindingFunction [ByteString]
type BindingPTR    = BindingFunction [ValidFQDN]
type BindingNS     = BindingFunction [ValidFQDN]
type BindingCNAME  = BindingFunction [ValidFQDN]
type BindingDNAME  = BindingFunction [ValidFQDN]
type BindingMX     = BindingFunction [(Int, ValidFQDN)]
type BindingSOA    = BindingFunction [(ValidFQDN, ValidFQDN, Int, Int, Int, Int, Int)]
type BindingSRV    = BindingFunction [(Int, Int, Int, ValidFQDN)]

-------------------------------------------------------------------------------
--                              Binding options                              --
-------------------------------------------------------------------------------

-- | a Wrapper for all the values read from the configuration
--
-- It embeds all the general information needed for debuging or to provide
-- a meaning error message (line/columns).
data Token value = Token
    { tokenLine   :: Int
    , tokenColumn :: Int
    , tokenValue  :: value
    } deriving (Show, Eq, Ord)

-- | An error wrapper
-- Mainly use in the Parser/Class to provide default error message formating
-- (a Token with its value and an error messasge)
data TokenError value = TokenError
    { failedToken :: Token value
    , failedMessage :: String
    } deriving (Show, Eq)

-- | Helper to formate a clean error in the case of Command Line parsing
--
-- This can be use in your Binding's getter functions while parsing the
-- command line to show clean and meaningful information about the error
commandParsingError :: CommandLine -- ^ The related command line
                    -> String      -- ^ The error message to print along the Parsing error
                    -> Dns a       -- ^ Always return the generated error message
commandParsingError cmdl errorMessage =
    errorDns $ printTokenError (TokenError (getCommandLineType cmdl) errorMessage)

-- | This is a little helper to print a Token Error
-- This could be use for debug/error printing purpose
printTokenError :: Show value => TokenError value -> String
printTokenError te =
    prefixMsg ++ " " ++ (failedMessage te) ++ " in the context of: " ++ (show $ tokenValue token)
  where
    token = failedToken te
    prefixMsg = "line(" ++ (show $ tokenLine token) ++ ") column(" ++ (show $ tokenColumn token) ++ ")"

-------------------------------------------------------------------------------
--                              Command Scope                                --
-------------------------------------------------------------------------------

-- | This is the general representation of the given value
data CommandScope = CommandScope
    { getCommandScopeName :: Token String -- ^ The command String
    , getCommandLines     :: [CommandLine]
    , getCommandSubScope  :: [CommandScope]
    } deriving (Show, Eq)

getCommandName :: CommandScope
               -> String
getCommandName scope = tokenValue $ getCommandScopeName scope

-------------------------------------------------------------------------------
--                              Command Line                                 --
-------------------------------------------------------------------------------

-- | This record represents a Command Line from the configuration file (the
-- bind file).
--
-- This value will be use to configure a DNS Handler to the appropriate Type
-- and Domain.
data CommandLine = CommandLine
    { getCommandLineType    :: Token TYPE      -- ^ The DNS TYPE to configure
    , getCommandLineFQDN    :: Token ValidFQDN -- ^ the domain to execute the action on
    , getCommandLineFlags   :: [Token String]  -- ^ Flags/configuration options
    , getCommandLineOptions :: Opts            -- ^ Key=Value options configuration
    } deriving (Show, Eq)

-- | Execute the given function with the Flags of the given Command Line
withCommandLineFlags :: CommandLine
                     -> ([String] -> a)
                     -> a
withCommandLineFlags cmdl f =
    f (map tokenValue $ getCommandLineFlags cmdl)

-- | Execute the given function with the Flags of the given Command Line
--
-- Attempt to read the Flags values
withCommandLineFlags' :: Read value
                      => CommandLine
                      -> (Either (TokenError String) [value] -> a)
                      -> a
withCommandLineFlags' cmdl f =
    f (mapM tokenReadEither $ getCommandLineFlags cmdl)
  where
    tokenReadEither t =
        case readEither $ tokenValue t of
            Left err -> Left (TokenError t err)
            Right v  -> Right v

-------------------------------------------------------------------------------
--                              Binding Options                              --
-------------------------------------------------------------------------------

-- | Binding options are Key Value pair
-- A string and a Token.
--
-- The value is still wrapped into a Token in order to keep the all information
-- we can about this value and allow the user to provide meaning error message.
newtype Opts = Opts
    { getOpts :: Map String (Token String)
    } deriving (Show, Eq)

emptyOpts :: Opts
emptyOpts = Opts Map.empty

toListOpts :: Opts -> [(String, Token String)]
toListOpts = Map.toList . getOpts

insertOpts :: String -> Token String -> Opts -> Opts
insertOpts k v m = Opts $ Map.insert k v (getOpts m)

-- | look up the given /key/ in the options /Opts/
-- and 'execute' the given function with the result
withOpt :: Opts   -- ^ the options to lookup in
        -> String -- ^ the /key/ to lookup
        -> (Maybe (Token String) -> a) -- ^ /f/
        -> a -- ^ (f $ lookup key options)
withOpt opts k f = f $ Map.lookup k $ getOpts opts
