-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Network.DNS.API.Bind.Class
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

module Network.DNS.API.Bind.Class
    ( -- * Binding class
      Binding(..)
      -- ** Helpers
    , notImplementedBinding
    , notSupportedBinding
    , commandParsingError
      -- * Default Binding
    , DefaultBinding(..)
    ) where

import           Control.Monad
import           Data.Byteable
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.IP (IPv4, IPv6)
import           Data.List (intercalate)
import           Text.Read
import           Network.DNS.API.Bind.Types
import           Network.DNS.API.Connection (Connection)
import           Network.DNS.API.Error
import           Network.DNS.API.FQDN

-- | This represent all Bindings a Binding Command Must implement
-- This bindings must provides return values corresponding to there types
--
-- The get/TYPE/ are the CommandLine Parsers related to the DNS /TYPE/.
class Binding binding where
    -- | This is the expected Command Scope Name
    --
    -- If the scope is empty, then all the command line which are
    -- not in the scope will be use for this Binding.
    --
    -- the list describe the binding scope recursivity
    -- For example, an option with a name:
    --
    -- > [ "std", "example", "binding" ]
    --
    -- will be trigger with the Bind file lines:
    --
    -- > # not in the command scope
    -- > @std {
    -- >   # not in the command scope
    -- >   @example {
    -- >     # not in the command scope
    -- >     @binding {
    -- >        # ADD THE Bind Lines HERE
    -- >     }
    -- >     # not in the command scope
    -- >   }
    -- >   # not in the command scope
    -- > }
    -- > # not in the command scope
    --
    getName   :: binding -> [String]

    -- | The documentation related to this Command Scope
    getHelp   :: binding -> [String]

    getA      :: binding -> CommandLine -> Dns BindingA
    getAAAA   :: binding -> CommandLine -> Dns BindingAAAA
    getTXT    :: binding -> CommandLine -> Dns BindingTXT
    getPTR    :: binding -> CommandLine -> Dns BindingPTR
    getNS     :: binding -> CommandLine -> Dns BindingNS
    getCNAME  :: binding -> CommandLine -> Dns BindingCNAME
    getDNAME  :: binding -> CommandLine -> Dns BindingDNAME

-- | Helper to formate a clean error in the case of Command Line parsing
--
-- This can be use in your Binding's getter functions while parsing the
-- command line to show clean and meaningful information about the error
commandParsingError :: CommandLine -- ^ The related command line
                    -> String      -- ^ The error message to print along the Parsing error
                    -> Dns a       -- ^ Always return the generated error message
commandParsingError cmdl errorMessage =
    errorDns $ printTokenError (TokenError (getCommandLineType cmdl) errorMessage)

-- | In case you don't want to implement an interface you can provide this function
-- This function can be use to fill the DNS records type you may not want to use/enable
-- in your DNS server.
--
-- It returns a default BindingFunction which always fail properly
notImplementedBinding :: Binding binding
                      => binding     -- ^ not use
                      -> CommandLine -- ^ not use
                      -> Dns (BindingFunction a)
notImplementedBinding _ _ = return $ BindingFunction $ \_ _ -> pureDns $ errorDns "error: not implemented"

notSupportedBinding :: Binding binding
                    => binding
                    -> CommandLine
                    -> Dns a
notSupportedBinding binding cmdl =
    commandParsingError cmdl ("This DNS Type is not supported in : " ++ (show $ intercalate "." (getName binding)))

-------------------------------------------------------------------------------
--                              Default Binding                              --
-------------------------------------------------------------------------------

-- | The default Bindig has no name
-- This is in order to not have to "scope" its commands into a Command scope
--
-- The default Bindings are Standards DNS Record Bindinds
--
-- These options allow you to define the classic, usual, DNS records.
-- Such as:
-- * A     <Domain Name> <IPv4>
-- * AAAA  <Domain Name> <IPv6>
-- * TXT   <Domain Name> <String|QuotedString> [ ' ' <String|QuotedString>]
--   * no more than 10 text messages
-- * NS    <Domain Name> <Domain Name> [ ' ' <Domain Name>]
--   * no more than 10 Domain names
-- * CNAME <Domain Name> <Domain Name>
-- * DNAME <Domain Name> <Domain Name>
-- * PTR   <Domain Name> <Domain Name>
--
data DefaultBinding = DefaultBinding
    deriving (Show, Eq)

instance Binding DefaultBinding where
    getName _ = []
    getHelp _ = [ "The default Bindings are Standards DNS Record Bindinds"
                , ""
                , "These options allow you to define the classic, usual, DNS records."
                , "Such as:"
                , "* A     <Domain Name> <IPv4>"
                , "* AAAA  <Domain Name> <IPv6>"
                , "* TXT   <Domain Name> <String|QuotedString> [ ' ' <String|QuotedString>]"
                , "* NS    <Domain Name> <Domain Name> [ ' ' <Domain Name>]"
                , "* CNAME <Domain Name> <Domain Name>"
                , "* DNAME <Domain Name> <Domain Name>"
                , "* PTR   <Domain Name> <Domain Name>"
                ]

    getA _ cmdl =
        withCommandLineValues cmdl $ \l ->
            case l of
                []        -> commandParsingError cmdl "expecting an IPv4 for this command Line"
                [ipv4str] -> do
                    ip <- either (commandParsingError cmdl) return $ readEither ipv4str :: Dns IPv4
                    return $ BindingFunction $ defaultBindingReturn [ip]
                _ -> commandParsingError cmdl "expecting only one Value"
    getAAAA _ cmdl =
        withCommandLineValues cmdl $ \l ->
            case l of
                []        -> commandParsingError cmdl "expecting an IPv6 for this command Line"
                [ipv6str] -> do
                    ip <- either (commandParsingError cmdl) return $ readEither ipv6str :: Dns IPv6
                    return $ BindingFunction $ defaultBindingReturn [ip]
                _ -> commandParsingError cmdl "expecting only one Value"
    getTXT   _ cmdl = defaultBindingTXT  cmdl 1 10
    getPTR   _ cmdl = defaultBindingFQDN cmdl 1 1
    getCNAME _ cmdl = defaultBindingFQDN cmdl 1 1
    getDNAME _ cmdl = defaultBindingFQDN cmdl 1 1
    getNS    _ cmdl = defaultBindingFQDN cmdl 1 10

checkFQDNEmpty :: FQDN fqdn => fqdn -> Bool
checkFQDNEmpty fqdn
    | byteableLength fqdn == 0 = True
    | otherwise                = False

defaultBindingReturn :: value -> Connection context -> ValidFQDN -> DnsIO value
defaultBindingReturn v _ fqdn
    | checkFQDNEmpty fqdn = return v
    | otherwise           = pureDns $ errorDns "error: void binding"

defaultBindingTXT :: CommandLine
                  -> Int
                  -> Int
                  -> Dns (BindingFunction [ByteString])
defaultBindingTXT cmdl minTxt maxTxt =
    withCommandLineValues cmdl $ \l -> do
        when (minTxt > length l) $ commandParsingError cmdl ("expecting at least " ++ show minTxt ++ " Text message(s)")
        when (maxTxt < length l) $ commandParsingError cmdl ("expecting at no more than " ++ show minTxt ++ " Text message(s)")
        return $ BindingFunction $ defaultBindingReturn $ map BC.pack l

defaultBindingFQDN :: CommandLine
                   -> Int
                   -> Int
                   -> Dns (BindingFunction [ValidFQDN])
defaultBindingFQDN cmdl minFqdn maxFqdn =
    withCommandLineValues cmdl $ \l -> do
        when (minFqdn > length l) $ commandParsingError cmdl ("expecting at least " ++ show minFqdn ++ " FQDN(s)")
        when (maxFqdn < length l) $ commandParsingError cmdl ("expecting at no more than " ++ show minFqdn ++ " FQDN(s)")
        fqdns <- mapM (validateFQDN . BC.pack) l
        return $ BindingFunction $ defaultBindingReturn fqdns
