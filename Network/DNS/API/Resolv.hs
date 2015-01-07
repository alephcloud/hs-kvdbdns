-- Copyright (c) 2013-2015 PivotCloud, Inc.
--
-- Network.DNS.API.Resolv
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

module Network.DNS.API.Resolv
    ( getFirstResolvSeed
      -- * DNS resolver
    , contactDNSResolver
    , contactDNSResolverAt
    , makeResolvSeedSafe
    ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Except
import           Data.Byteable
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Hourglass.Types
import qualified Network.DNS as DNS
import           Network.DNS.API.Error
import           Network.DNS.API.FQDN
import           Network.Socket (PortNumber, HostName)

-- catch all exception
catchAny :: IO a -> (SomeException -> DnsIO a) -> DnsIO a
catchAny action alternativeAction = do
    res <- liftIO $ tryAny action
    case res of
        Left  e -> alternativeAction e
        Right a -> return a
  where
    tryAny :: IO a -> IO (Either SomeException a)
    tryAny = Control.Exception.try

-- | Try the list of ResolvConf and stop at the first working one
getFirstResolvSeed :: [DNS.ResolvConf] -> DnsIO DNS.ResolvSeed
getFirstResolvSeed [] = pureDns $ errorDns $ "Network.DNS.API.Resolv.getFirstResolvSeed: no Safe ResolvConf"
getFirstResolvSeed (rc:rcs) = do
    catchAny
        (DNS.makeResolvSeed rc)
        (\_ -> getFirstResolvSeed rcs)

-- | This function is to contact a specific DNS Resolver using a Domain Name
contactDNSResolver :: FQDN fqdn
                   => fqdn             -- ^ the DNS Server to contact
                   -> Maybe PortNumber -- ^ port number
                   -> Maybe Seconds    -- ^ timeout
                   -> Maybe Int        -- ^ retry
                   -> DnsIO DNS.ResolvSeed
contactDNSResolver fqdn mport mto mr = do
    rs <- catchAny
            (DNS.makeResolvSeed resolvConf)
            (\ex -> pureDns $ errorDns $ errorMsg $ "error while creating the ResolvSeed: " ++ show ex)
    (tryLookupWith rs DNS.lookupA) <|> (tryLookupWith rs DNS.lookupAAAA)
  where
    errorMsg :: String -> String
    errorMsg str = "Network.DNS.API.Client.contactDNSResovler: " ++ str

    resolvConf :: DNS.ResolvConf
    resolvConf = let r1 = DNS.defaultResolvConf
                     r2 = maybe r1 (\(Seconds to) -> r1 { DNS.resolvTimeout = (fromIntegral to) * 3000 * 3000 }) mto
                 in maybe r2 (\r  -> r2 { DNS.resolvRetry = r }) mr

    tryLookupWith :: (Show output)
                  => DNS.ResolvSeed
                  -> (DNS.Resolver -> DNS.Domain -> IO (Either DNS.DNSError [output]))
                  -> DnsIO DNS.ResolvSeed
    tryLookupWith rs lookupFunction = do
        result <- liftIO $ DNS.withResolver rs $ \resolver -> lookupFunction resolver (toBytes fqdn)
        case result of
            Right [] -> pureDns $ errorDns $ errorMsg $ "no server to contact at this domain: " ++ (show $ toBytes fqdn)
            Right l  -> tryUntilSuccess
                            (\a -> contactDNSResolverAt (show a) mport mto mr)
                            (pureDns $ errorDns $ errorMsg "non of the IPv4 address worked for this domain " ++ (show $ toBytes fqdn))
                            l
            Left err -> pureDns $ errorDns $ errorMsg $ "error while lookupA: " ++ show err

-- | Attempt to create a resolv seed which contact the given hostname
contactDNSResolverAt :: HostName         -- ^ an ip address (IPv6 or IPv4)
                     -> Maybe PortNumber -- ^ port number
                     -> Maybe Seconds    -- ^ timeout
                     -> Maybe Int        -- ^ retry
                     -> DnsIO DNS.ResolvSeed
contactDNSResolverAt hostname mport mto mr =
    catchAny
         (DNS.makeResolvSeed $ resolvConf { DNS.resolvInfo = resolvInfo })
         (\ex -> pureDns $ errorDns $ errorMsg $ "could not create a resolv seed from the received IP: " ++ show hostname ++ " error is: " ++ show ex)
  where
    errorMsg :: String -> String
    errorMsg str = "Network.DNS.API.Client.contactDNSResovlerAt: " ++ str

    resolvInfo :: DNS.FileOrNumericHost
    resolvInfo = maybe (DNS.RCHostName hostname) (\p -> DNS.RCHostPort hostname p) mport
    resolvConf :: DNS.ResolvConf
    resolvConf = let r1 = DNS.defaultResolvConf
                     r2 = maybe r1 (\(Seconds to) -> r1 { DNS.resolvTimeout = (fromIntegral to) * 3000 * 3000 }) mto
                 in maybe r2 (\r  -> r2 { DNS.resolvRetry = r }) mr

-- | Create the ResolvSeed using the given parameters
--
-- Will first attempt to create a ResolvSeed with the HostName information
-- (The DNS Server address and the port) if provided.
-- We will try any IPv4 or IPv6 available from the given domain name
--
-- If still not found, we will then fall back to the default DNS Resolver
-- (/etc/resolv.conf in linux/mac).
--
-- If it still fails, then fall back to a more generic DNS configuration by contacting NS Resolver at address 8.8.8.8
makeResolvSeedSafe :: Maybe ByteString -- ^ the DNS Server to contact (Domain Name, IPv4 or IPv6)
                   -> Maybe PortNumber -- ^ port number (useless without the DNS Server to contact)
                   -> Maybe Seconds    -- ^ timeout
                   -> Maybe Int        -- ^ retry
                   -> DnsIO DNS.ResolvSeed
makeResolvSeedSafe mfqdn mport mto mr = do
    attemptToContactAGlobalResolver <|> attemptToContactTheDefaultResolvers
  where
    attemptToContactTheDefaultResolvers :: DnsIO DNS.ResolvSeed
    attemptToContactTheDefaultResolvers =
        getFirstResolvSeed
            [ resolvConf
            , resolvConf { DNS.resolvInfo = DNS.RCHostName "8.8.8.8" }
            ]

    attemptToContactAGlobalResolver :: DnsIO DNS.ResolvSeed
    attemptToContactAGlobalResolver =
        maybe (pureDns $ errorDns $ errorMsg "no Domain Name provided, fall back to the local resolv configuration")
              (\fqdn -> (contactDNSResolverAt (BC.unpack fqdn) mport mto mr) <|> (contactDNSResolver fqdn mport mto mr))
              mfqdn

    errorMsg :: String -> String
    errorMsg str = "Network.DNS.API.Client.makeResolvSeedSafe: " ++ str

    resolvConf :: DNS.ResolvConf
    resolvConf = let r1 = DNS.defaultResolvConf
                     r2 = maybe r1 (\(Seconds to) -> r1 { DNS.resolvTimeout = (fromIntegral to) * 3000 * 3000 }) mto
                 in maybe r2 (\r  -> r2 { DNS.resolvRetry = r }) mr
