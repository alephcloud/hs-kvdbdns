-- |
-- Module      : Network.DNS.API.Bind
-- License     : BSD-Style
-- Copyright   : Copyright © 2014 AlephCloud Systems, Inc.
--
-- Maintainer  : Nicolas DI PRIMA <ndiprima@pivotmail.com>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE RankNTypes #-}
module Network.DNS.API.Bind
    ( -- * Main function
      DNSBindings(..)
    , emptyDNSBindings
    , insertDNSBindings

    , DefaultBinding(..)

      -- * Bind Configuration
    , BindingLine
    , parseBindFile
    , parseBindingLines
    , parseBindingLine

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

import           Control.Applicative
import           Data.Byteable
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Char (isAlpha, isAlphaNum)
import           Data.IP (IPv4, IPv6)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.String.Parse (Parser, Result(..))
import qualified Data.String.Parse as P
import           Network.DNS (TYPE(..))
import           Network.DNS.API.FQDN
import           Network.DNS.API.Error
import           Network.DNS.API.Connection(Connection)

-------------------------------------------------------------------------------
--                              Binding options                              --
-------------------------------------------------------------------------------

newtype Opts = Opts
    { getOpts :: Map String String
    } deriving (Show, Eq)

insertOpts :: String -> String -> Opts -> Opts
insertOpts k v m = Opts $ Map.insert k v (getOpts m)

emptyOpts :: Opts
emptyOpts = Opts Map.empty

withSafeOpt :: Opts -> String -> (Maybe String -> a) -> a
withSafeOpt opts k f = f $ Map.lookup k $ getOpts opts

withUnsafeOpt :: Opts -> String -> (String -> a) -> a
withUnsafeOpt opts k f =
    case Map.lookup k $ getOpts opts of
        Nothing -> error $ "Network.DNS.API.Bind: expected options: " ++ k
        Just v  -> f v

-------------------------------------------------------------------------------
--                              Parser                                       --
-------------------------------------------------------------------------------

data BindingLine = BindingLine
    { getLineType :: TYPE
    , getLineFQDN :: ValidFQDN
    , getLineCommand :: String
    , getLineOptions :: Opts
    } deriving (Show, Eq)

newtype BindingFile = BindingFile [BindingLine]
    deriving (Show, Eq)

parseBindFile :: FilePath  -> IO (Either String [BindingLine])
parseBindFile filepath = do
    content <- lines <$> readFile filepath
    return $ parseBindingLines content

parseBindingLines :: [String] -> Either String [BindingLine]
parseBindingLines l = catMaybes <$> mapM parseBindingLine l

parseBindingLine :: String -> Either String (Maybe BindingLine)
parseBindingLine l
    | null l = Right Nothing
    | otherwise =
        case P.parse bindingOrCommentParser l of
            ParseOK   _ v -> Right v
            ParseFail err -> Left $ "Networl.DNS.API.Bind: parseBindlingLine: " ++ err
            ParseMore f   ->
                -- Force the state termination
                case f "\n" of
                    ParseOK _ v -> Right v
                    ParseFail err -> Left $ "Networl.DNS.API.Bind: parseBindlingLine: " ++ err
                    _             -> Left $ "Networl.DNS.API.Bind: parseBindlingLine: cannot finish the state"

bindingOrCommentParser :: Parser (Maybe BindingLine)
bindingOrCommentParser =
    (Just <$> bindingLineParser) <|> (commentLineParser >> return Nothing)

commentLineParser :: Parser ()
commentLineParser = skipSpaces >> P.char '#'

bindingLineParser :: Parser BindingLine
bindingLineParser = do
    skipSpaces
    t <- typeParser
    skipSpaces
    fqdn <- fqdnParser
    skipSpaces
    name <- P.takeWhile isAlphaNum
    skipSpaces
    opts <- optsParser
    return $ BindingLine t fqdn name opts

skipSpaces :: Parser ()
skipSpaces = P.skipWhile (\c -> c `elem` " \t")

typeParser :: Parser TYPE
typeParser = do
    str <- P.takeWhile isAlpha
    case str of
        "A"     -> return A
        "AAAA"  -> return AAAA
        "TXT"   -> return TXT
        "NS"    -> return NS
        "CNAME" -> return CNAME
        "DNAME" -> return DNAME
        "PTR"   -> return PTR
        "MX"    -> fail "MX not supported yet"
        "SOA"   -> fail "SOA not supported yet"
        "SRV"   -> fail "SRV not supported yet"
        _       -> fail $ "TYPE '" ++ str ++ "' not supported"

fqdnParser :: Parser ValidFQDN
fqdnParser = do
    str <- P.takeWhile (\c -> isAlphaNum c || c `elem` "-.")
    case execDns $ validateFQDN $ BC.pack str of
        Left  err  -> fail $ "invalid FQDN: " ++ err
        Right fqdn -> return fqdn

-- | Opts Parser
--
-- Will read a opts section
-- will stop at an end of line, a white space or a tabulation
--
-- Grammar:
--  options: "none" | optionsList
--  optionsList: optionSome | optionOne
--  optionSome: optionOne ',' optionsList
--  optionOne: key '=' <value>
--  key: [a-zA-Z0-9]+
--  value: [a-zA-Z0-9:./-_]+ | '"' .* '"'
--
-- Example:
-- * ip=127.0.0.1
-- * fqdn=domain.net.,priority=5
-- * ip=::1
optsParser :: Parser Opts
optsParser = optNoneParser <|> optListParser emptyOpts

optNoneParser :: Parser Opts
optNoneParser = P.string "none" >> return emptyOpts

optListParser :: Opts -> Parser Opts
optListParser opts = do
    opts'<- optParseOne opts
    (P.char charSeparator >> optListParser opts') <|> return opts'

optParseOne :: Opts -> Parser Opts
optParseOne opts = do
    (k, v) <- optKeyValParser
    return $ insertOpts k v opts

charAssign :: Char
charAssign = '='

charSeparator :: Char
charSeparator = ','

isKeyChar :: Char -> Bool
isKeyChar = isAlphaNum

isValueChar :: Char -> Bool
isValueChar c = (isAlphaNum c) || (c `elem` ":./-_")

quotedStringParser :: Parser String
quotedStringParser = do
    P.char '"'
    str <- P.takeWhile ((/=) '"')
    P.char '"'
    return str

optKeyValParser :: Parser (String, String)
optKeyValParser = do
    key <- P.takeWhile isKeyChar
    P.char charAssign
    value <- quotedStringParser <|> (P.takeWhile isValueChar)
    return (key, value)

-------------------------------------------------------------------------------
--                              Binding class                                --
-------------------------------------------------------------------------------

class Binding binding where
    -- The function to use to parse a 
    getName   :: binding -> String

    getA      :: binding -> Opts -> Dns BindingA
    getAAAA   :: binding -> Opts -> Dns BindingAAAA
    getTXT    :: binding -> Opts -> Dns BindingTXT
    getPTR    :: binding -> Opts -> Dns BindingPTR
    getNS     :: binding -> Opts -> Dns BindingNS
    getCNAME  :: binding -> Opts -> Dns BindingCNAME
    getDNAME  :: binding -> Opts -> Dns BindingDNAME

-- | In case you don't want to implement an interface you can provide this function
notImplementedBinding :: Binding binding => binding -> Opts -> Dns (BindingFunction a)
notImplementedBinding _ _ = return $ BindingFunction $ \_ _ -> pureDns $ errorDns "error: not implemented"

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
--                              Bindings                                     --
-------------------------------------------------------------------------------

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
        A    -> insertDNSBindingA binding bl dnsbs
        AAAA -> insertDNSBindingAAAA binding bl dnsbs 
        TXT -> insertDNSBindingTXT binding bl dnsbs 
        PTR -> insertDNSBindingPTR binding bl dnsbs 
        NS -> insertDNSBindingNS binding bl dnsbs 
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

-------------------------------------------------------------------------------
--                              Default Binding                              --
-------------------------------------------------------------------------------

data DefaultBinding = DefaultBinding
    deriving (Show, Eq)

instance Binding DefaultBinding where
    getName _ = "default"

    getA _ opts =
        withUnsafeOpt opts "ip" $ \str ->
            let ip = read str :: IPv4
            in  return $ BindingFunction $ defaultBindingReturn [ip]
    getAAAA _ opts =
        withUnsafeOpt opts "ip" $ \str ->
            let ip = read str :: IPv6
            in  return $ BindingFunction $ defaultBindingReturn [ip]
    getTXT _ opts =
        withUnsafeOpt opts "text" $ \str ->
            let bs = BC.pack str
            in  return $ BindingFunction $ defaultBindingReturn [bs]
    getPTR   _ = defaultBindingFQDN
    getNS    _ = defaultBindingFQDN
    getCNAME _ = defaultBindingFQDN
    getDNAME _ = defaultBindingFQDN

checkFQDNEmpty :: FQDN fqdn => fqdn -> Bool
checkFQDNEmpty fqdn
    | byteableLength fqdn == 0 = True
    | otherwise                = False

defaultBindingReturn :: value -> Connection context -> ValidFQDN -> DnsIO value
defaultBindingReturn v _ fqdn
    | checkFQDNEmpty fqdn = return v
    | otherwise           = pureDns $ errorDns "error: void binding"

defaultBindingFQDN :: Opts -> Dns (BindingFunction [ValidFQDN])
defaultBindingFQDN opts =
    withUnsafeOpt opts "fqdn" $ \str -> do
        fqdn <- validateFQDN $ BC.pack str
        return $ BindingFunction $ defaultBindingReturn [fqdn]

