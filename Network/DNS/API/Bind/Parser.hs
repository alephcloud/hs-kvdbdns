-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Network.DNS.API.Bind.Parser
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

module Network.DNS.API.Bind.Parser
    ( BindingLine(..)
    , parseBindFile
    ) where

import           Control.Applicative
import           Data.Char (isAlpha, isAlphaNum, toUpper)
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe (catMaybes)
import           Data.String.Parse (Parser, Result(..))
import qualified Data.String.Parse as P
import           Network.DNS.Types (TYPE(..))
import           Network.DNS.API.Bind.Types
import           Network.DNS.API.FQDN
import           Network.DNS.API.Error

-------------------------------------------------------------------------------
--                              Parser                                       --
-------------------------------------------------------------------------------

-- | represent a Bind line
data BindingLine = BindingLine
    { getLineType    :: !TYPE
    , getLineFQDN    :: !ValidFQDN
    , getLineCommand :: !String
    , getLineOptions :: !Opts
    } deriving (Show, Eq)

-- | Parse a Bind file
parseBindFile :: FilePath -- ^ file path to the bind file
              -> IO (Either String [BindingLine])
parseBindFile filepath = do
    content <- zip [1..] . lines <$> readFile filepath
    return $ parseBindingLines content

parseBindingLines :: [(Int, String)] -> Either String [BindingLine]
parseBindingLines l = catMaybes <$> mapM parseBindingLine l

parseBindingLine :: (Int, String) -> Either String (Maybe BindingLine)
parseBindingLine (line, l)
    | null l = Right Nothing
    | otherwise =
        case P.parse bindingOrCommentParser l of
            ParseOK   _ v -> Right v
            ParseFail err -> Left $ errorMsg err
            ParseMore f   ->
                -- Force the state termination
                case f "\n" of
                    ParseOK _ v -> Right v
                    ParseFail err -> Left $ errorMsg err
                    _             -> Left $ errorMsg "nonterminal state"
  where
    errorMsg :: String -> String
    errorMsg msg = "Network.DNS.API.Bind.Parser.parseBindingLine: line(" ++ show line ++ "): " ++ msg

bindingOrCommentParser :: Parser (Maybe BindingLine)
bindingOrCommentParser =
    (commentLineParser >> return Nothing)
    <|> (Just <$> bindingLineParser)

commentLineParser :: Parser ()
commentLineParser = skipSpaces >> P.char '#'

bindingLineParser :: Parser BindingLine
bindingLineParser = do
    BindingLine
        <$> typeParser
        <*> fqdnParser
        <*> commandParser
        <*> optsParser

commandParser :: Parser String
commandParser = do
    skipSpaces
    str <- takeUntilSpaces
    if and $ map isAlphaNum str
        then return str
        else fail $ "a command can only be an AlphaNum"

skipSpaces :: Parser ()
skipSpaces = P.skipWhile (\c -> c `elem` " \t")

takeUntilSpaces :: Parser String
takeUntilSpaces = P.takeWhile (not . flip elem " \t")

typeParser :: Parser TYPE
typeParser = do
    skipSpaces
    str <- takeUntilSpaces
    if and $ map isAlpha str
        then switchDNSType str
        else fail $ "a DNS Type can only be an Alpha"
  where
    switchDNSType :: String -> Parser TYPE
    switchDNSType str =
        case map toUpper str of
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
    skipSpaces
    str <- takeUntilSpaces
    either (fail . (++) "invalid FQDN: ") return $ execDns $ validateFQDN $ BC.pack str

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
optsParser = do
    skipSpaces
    optNoneParser <|> optListParser emptyOpts

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
