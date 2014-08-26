-- |
-- Module      : Network.DNS.KVDB.Server
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@alephcloud.com>
-- Stability   : experimental
-- Portability : unknown
--
module Network.DNS.KVDB.Server
  ( ServerConf(..)
  , handleQuery
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as SL (toChunks, fromChunks)
import Data.Default
import Data.Maybe  (listToMaybe)
import Data.Monoid (mconcat)

import Network.DNS hiding (lookup)
import qualified Network.DNS.KVDB.Types as KVDB

------------------------------------------------------------------------------
--                         Server Configuration                             --
------------------------------------------------------------------------------

-- | Server configuration
data ServerConf = ServerConf
  { query   :: ByteString -> IO (Maybe ByteString) -- ^ the method to perform a request
  , inFail  :: DNSFormat  -> IO (Either String DNSFormat) -- ^ the method to use to handle query failure
  }

-- Default
instance Default ServerConf where
    def = ServerConf
      { query   = queryNothing
      , inFail  = inFailUndefined
      }

-- | Default implementation of a query
queryNothing :: ByteString -> IO (Maybe ByteString)
queryNothing _ = return Nothing

-- | Default implementation of an inFail
inFailUndefined :: DNSFormat -> IO (Either String DNSFormat)
inFailUndefined _ = return $ Left "command undefined"

------------------------------------------------------------------------------
--                         The main function                                --
------------------------------------------------------------------------------

splitTxt :: ByteString -> [ByteString]
splitTxt bs
  | B.length bs < 255 = [bs]
  | otherwise = node:(splitTxt xs)
  where
    (node, xs) = B.splitAt 255 bs

-- Handle a request:
-- try the query function given in the ServerConf
-- if it fails, then call the given proxy
handleRequest :: ServerConf -> DNSFormat -> IO (Either String ByteString)
handleRequest conf req =
  case listToMaybe . filterTXT . question $ req of
    Just q -> do
        mres <- query conf $ qname q
        case mres of
           Just txt -> return $ Right $ mconcat . SL.toChunks $ encode $ responseTXT q (splitTxt txt)
           Nothing  -> inFail conf req >>= return.inFailWrapper
    Nothing -> inFail conf req >>= return.inFailWrapper
  where
    filterTXT = filter ((==TXT) . qtype)

    ident :: Int
    ident = identifier . header $ req

    inFailWrapper :: Either String DNSFormat -> Either String ByteString
    inFailWrapper (Right r) = Right $ mconcat . SL.toChunks $ encode r
    inFailWrapper (Left er) = Left er

    responseTXT :: Question -> [ByteString] -> DNSFormat
    responseTXT q l =
      let hd = header defaultResponse
          dom = qname q
          al = map (\txt -> ResourceRecord dom TXT 0 (B.length txt) (RD_TXT txt)) l
      in  defaultResponse
            { header = hd { identifier = ident, qdCount = 1, anCount = length al }
            , question = [q]
            , answer = al
            }

    -- imported from dns:Network/DNS/Internal.hs
    defaultQuery :: DNSFormat
    defaultQuery = DNSFormat {
        header = DNSHeader {
           identifier = 0
         , flags = DNSFlags {
               qOrR         = QR_Query
             , opcode       = OP_STD
             , authAnswer   = False
             , trunCation   = False
             , recDesired   = True
             , recAvailable = False
             , rcode        = NoErr
             }
         , qdCount = 0
         , anCount = 0
         , nsCount = 0
         , arCount = 0
         }
      , question   = []
      , answer     = []
      , authority  = []
      , additional = []
      }

    -- imported from dns:Network/DNS/Internal.hs
    defaultResponse :: DNSFormat
    defaultResponse =
      let hd = header defaultQuery
          flg = flags hd
      in  defaultQuery {
            header = hd {
              flags = flg {
                  qOrR = QR_Response
                , authAnswer = True
                , recAvailable = True
                }
        }
      }

-- | handle a DNS query
handleQuery :: ServerConf
            -> ByteString -- ^ the query
            -> IO (Either String ByteString)
handleQuery conf bs =
  case decode (SL.fromChunks [bs]) of
    Left msg  -> return $ Left $ "Query: Error: " ++ msg
    Right req -> handleRequest conf req
