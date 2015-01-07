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
    ) where

import           Control.Exception
import           Control.Monad.Except
import qualified Network.DNS as DNS
import           Network.DNS.API.Error

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
