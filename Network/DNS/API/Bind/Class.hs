-- Copyright (c) 2013-2015 PivotCloud, Inc.
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

module Network.DNS.API.Bind.Class
    ( -- * Binding class
      Binding(..)
      -- ** Helpers
    , printBindingHelp
    , notImplementedBinding
    , notSupportedBinding
    , commandParsingError
      -- * Default Binding
    , DefaultBinding(..)
    ) where

import           Control.Applicative
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
    getMX     :: binding -> CommandLine -> Dns BindingMX
    getSOA    :: binding -> CommandLine -> Dns BindingSOA
    getSRV    :: binding -> CommandLine -> Dns BindingSRV

printBindingHelp :: Binding binding
                 => String
                 -> binding
                 -> String
printBindingHelp indentation binding =
    intercalate "\n" $ bindingPath:linesDoc
  where
    bindingPath :: String
    bindingPath = indentation ++ (intercalate "/" $ getName binding)

    linesDoc :: [String]
    linesDoc = map ((++) (indentation ++ "  ")) $ getHelp binding

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
                , "* MX    <Domain Name> <int> <Domain Name> [ ' ' <int> <Domain Name>]"
                , "* SRV   <Domain Name> <int> <int> <int> <Domain Name>"
                , "* SOA   <Domain Name> <Domain Name> <Domain Name> <int> <int> <int> <int> <int>"
                ]

    getA _ cmdl =
        withCommandLineFlags cmdl $ \l ->
            case l of
                []        -> commandParsingError cmdl "expecting an IPv4 for this command Line"
                [ipv4str] -> do
                    ip <- readDns ipv4str cmdl :: Dns IPv4
                    return $ BindingFunction $ defaultBindingReturn [ip]
                _ -> commandParsingError cmdl "expecting only one Value"
    getAAAA _ cmdl =
        withCommandLineFlags cmdl $ \l ->
            case l of
                []        -> commandParsingError cmdl "expecting an IPv6 for this command Line"
                [ipv6str] -> do
                    ip <- readDns ipv6str cmdl :: Dns IPv6
                    return $ BindingFunction $ defaultBindingReturn [ip]
                _ -> commandParsingError cmdl "expecting only one Value"
    getSOA   _ cmdl =
        withCommandLineFlags cmdl $ \l ->
            case l of
                [auth, admEmail, serialNumStr, refreshStr, retryNumStr, authTTLStr, negTTLStr] -> do
                    authFQDN     <- validateFqdn auth cmdl
                    admEmailFQDN <- validateFqdn admEmail cmdl
                    serialNum    <- readDns serialNumStr cmdl
                    refresh      <- readDns refreshStr cmdl
                    retryNum     <- readDns retryNumStr cmdl
                    authTTL      <- readDns authTTLStr cmdl
                    negTTL       <- readDns negTTLStr cmdl
                    return $ BindingFunction $ defaultBindingReturn [(authFQDN, admEmailFQDN, serialNum, refresh, retryNum, authTTL, negTTL)]
                _ -> commandParsingError cmdl "expecting Flags: <domain> <domain> <int> <int> <int> <int> <int>"
    getSRV   _ cmdl =
        withCommandLineFlags cmdl $ \l ->
            case l of
                [priorityStr, weightStr, portStr, domainStr] -> do
                    priorityNum <- readDns priorityStr cmdl
                    weightNum   <- readDns weightStr cmdl
                    portNum     <- readDns portStr cmdl
                    domainFQDN  <- validateFqdn domainStr cmdl
                    return $ BindingFunction $ defaultBindingReturn [(priorityNum, weightNum, portNum, domainFQDN)]
                _ -> commandParsingError cmdl "expecting Flags: <int> <int> <int> <domain>"
    getTXT   _ cmdl = defaultBindingTXT  cmdl 1 10
    getPTR   _ cmdl = defaultBindingFQDN cmdl 1 1
    getCNAME _ cmdl = defaultBindingFQDN cmdl 1 1
    getDNAME _ cmdl = defaultBindingFQDN cmdl 1 1
    getNS    _ cmdl = defaultBindingFQDN cmdl 1 10
    getMX    _ cmdl = defaultBindingMX   cmdl 1 10

checkFQDNEmpty :: FQDN fqdn => fqdn -> Bool
checkFQDNEmpty fqdn
    | byteableLength fqdn == 0 = True
    | otherwise                = False

defaultBindingReturn :: value -> Connection -> ValidFQDN -> DnsIO value
defaultBindingReturn v _ fqdn
    | checkFQDNEmpty fqdn = return v
    | otherwise           = pureDns $ errorDns "error: void binding"

defaultBindingTXT :: CommandLine
                  -> Int
                  -> Int
                  -> Dns (BindingFunction [ByteString])
defaultBindingTXT cmdl minTxt maxTxt =
    withCommandLineFlags cmdl $ \l -> do
        when (minTxt > length l) $ commandParsingError cmdl ("expecting at least " ++ show minTxt ++ " Text message(s)")
        when (maxTxt < length l) $ commandParsingError cmdl ("expecting at no more than " ++ show minTxt ++ " Text message(s)")
        return $ BindingFunction $ defaultBindingReturn $ map BC.pack l

defaultBindingMX :: CommandLine
                 -> Int
                 -> Int
                 -> Dns (BindingFunction [(Int, ValidFQDN)])
defaultBindingMX cmdl minMX maxMX =
    withCommandLineFlags cmdl $ \l -> do
        when (minMX > length l) $ commandParsingError cmdl ("expecting at least " ++ show minMX ++ " MX entry(s)")
        when (maxMX < length l) $ commandParsingError cmdl ("expecting at no more than " ++ show minMX ++ " MX entry(s)")
        list <- readTwoByTwo l
        return $ BindingFunction $ defaultBindingReturn list
  where
    readTwoByTwo :: [String] -> Dns [(Int, ValidFQDN)]
    readTwoByTwo []  = return []
    readTwoByTwo [_] = commandParsingError cmdl "expecting Flags to run 2 by 2 (an integer and a ValidFQDN)"
    readTwoByTwo (w:dom:xs) = do
        weight <- readDns w cmdl
        fqdn   <- validateFqdn dom cmdl
        (:) (weight, fqdn) <$> readTwoByTwo xs

readDns :: Read value
        => String
        -> CommandLine
        -> Dns value
readDns str cmdl =
    case readEither str of
        Left err -> commandParsingError cmdl ("cannot read value " ++ show str ++ ": " ++ err)
        Right v  -> return v

validateFqdn :: String
             -> CommandLine
             -> Dns ValidFQDN
validateFqdn str cmdl =
    case execDns $ validateFQDN $ BC.pack str of
        Left err -> commandParsingError cmdl ("invalid Domain Name " ++ show str ++ ": " ++ err)
        Right v  -> return v

defaultBindingFQDN :: CommandLine
                   -> Int
                   -> Int
                   -> Dns (BindingFunction [ValidFQDN])
defaultBindingFQDN cmdl minFqdn maxFqdn =
    withCommandLineFlags cmdl $ \l -> do
        when (minFqdn > length l) $ commandParsingError cmdl ("expecting at least " ++ show minFqdn ++ " FQDN(s)")
        when (maxFqdn < length l) $ commandParsingError cmdl ("expecting at no more than " ++ show minFqdn ++ " FQDN(s)")
        fqdns <- mapM (flip validateFqdn cmdl) l
        return $ BindingFunction $ defaultBindingReturn fqdns
