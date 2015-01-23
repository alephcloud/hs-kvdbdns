-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Network.DNS.API.Bind.Parser2
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
    ( parseBindFile
    , prettyPrintCommandScope

    , Value(..)
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Byteable
import qualified Data.ByteString.Char8 as BC
import           Network.DNS.Types (TYPE(..))
import           Network.DNS.API.FQDN
import           Network.DNS.API.Bind.Types
import           Text.Read

-------------------------------------------------------------------------------
--                          Parsing Token Types                              --
-------------------------------------------------------------------------------

-- | This represents the expected type of Object to lex/parse from a Bind File
data Value =
      Comment String -- ^ a comment line
    | Command        -- ^ a Command symbol '@'
    | NewLine        -- ^ A line separator '\n'
    | OpenScope      -- ^ an open bracket '{'
    | CloseScope     -- ^ a close bracket '}'
    | ContinueLine   -- ^ an escape character '\'
    | Value String   -- ^ some Quoted or unquoted string
    | Assign         -- ^ '='
    | Other String   -- ^ unexpected data
    deriving (Show, Eq)

isTokenNewLine :: Token Value -> Bool
isTokenNewLine = (==) NewLine . tokenValue

isTokenComment :: Token Value -> Bool
isTokenComment token =
    case tokenValue token of
        Comment _ -> True
        _         -> False

-------------------------------------------------------------------------------
--                                  Parser                                   --
-------------------------------------------------------------------------------

-- | This little helper help you to re-print a CommandScope entirely
-- (All comments and indentation will be removed) but the command line options
-- the configuration and the values are printed and can be parsed again by the
-- Bind Parsers.
prettyPrintCommandScope :: CommandScope -> String
prettyPrintCommandScope scope = do
    (intercalate "\n" $ map (prettyPrintLines "") (getCommandLines scope))
    ++ (intercalate "\n" $ map (prettyPrintScope "") (getCommandSubScope scope))

prettyPrintScope :: String -> CommandScope -> String
prettyPrintScope indent scope = do
    (indent ++ "@" ++ scopeName ++ " {\n")
    ++ (intercalate "\n" $ map (prettyPrintLines (indent ++ "  ")) (getCommandLines scope))
    ++ (intercalate "\n" $ map (prettyPrintScope (indent ++ "  ")) (getCommandSubScope scope))
    ++ (indent ++ "}\n")
  where
    scopeName :: String
    scopeName = tokenValue $ getCommandScopeName scope

prettyPrintLines :: String -> CommandLine -> String
prettyPrintLines indent cl =
       indent ++ (show $ tokenValue $ getCommandLineType cl)
    ++ " "    ++ (BC.unpack $ toBytes $ tokenValue $ getCommandLineFQDN cl)
    ++ " "    ++ (intercalate " " $ map (\t -> "\"" ++ tokenValue t ++ "\"") $ getCommandLineFlags cl)
    ++ " "    ++ (intercalate " " $ map concatAssign $ toListOpts $ getCommandLineOptions cl)
  where
    concatAssign :: (String, Token String) -> String
    concatAssign (k, v) =
        "\"" ++ k ++ "\"=\"" ++ tokenValue v ++ "\""

-------------------------------------------------------------------------------
--                                  Parser                                   --
-------------------------------------------------------------------------------

-- | Parse a Bind File
--
-- This function can throw IO Exception due to use of Prelude.readFile
parseBindFile :: FilePath -- ^ filepath to the bind file
              -> IO (Either String CommandScope)
parseBindFile filepath = do
    content <- lines <$> readFile filepath
    return $ case linesToToken content of
        Left err -> Left $ printTokenError err
        Right v  ->
            case parseTokens v (CommandScope (Token 0 0 "") [] []) of
                Left err -> Left $ printTokenError err
                Right (sc, []) -> Right sc
                Right (_, l)   ->
                    Left $ printTokenError $ TokenError (head l) ("unexpected token in file: " ++ show filepath)

-------------------------------------------------------------------------------
--                               Parse the tokens                            --
-------------------------------------------------------------------------------

-- helper to convert a given token type to another one
-- but while keeping the line and column info
--
-- This function is use to replace the content of a Token by another kind of
-- value in the parser (from the token list to the CommandScope).
tokenToToken :: Token a
             -> b
             -> Token b
tokenToToken a b =
    Token (tokenLine a) (tokenColumn a) b

-- | Parse the given token list
-- and update the given CommandScope with the token values found in the list
parseTokens :: [Token Value]
            -> CommandScope
            -> Either (TokenError Value) (CommandScope, [Token Value])
parseTokens []     scope = Right (scope, [])
parseTokens (t:ts) scope =
    case tokenValue t of
        -- Ignore comments
        Comment _  -> parseTokens ts scope
        -- If found a command Symbol, then create a subScope
        Command    -> do
            when (null ts) $ Left $ TokenError t "expecting a command symbol"
            (subscope, ts') <- parseTokenCommand ts
            parseTokens ts' (scope { getCommandSubScope = subscope:(getCommandSubScope scope) })
        -- NewLine
        NewLine      -> parseTokens ts scope
        Value _      -> do
            (commandLine, ts') <- parseCommandLine (t:ts)
            parseTokens ts' (scope { getCommandLines = commandLine:(getCommandLines scope) })

        -- The Command Scope just ended... return the scope
        CloseScope   -> Right (scope, ts)

        -- Error in Context of the configuration file
        ContinueLine -> Left $ TokenError t "unexpected ContinueLine symbol '\\'"
        OpenScope    -> Left $ TokenError t "unexpected in context (only to open a Scoped Command)"
        Assign       -> Left $ TokenError t "unexpected in context (for Key value assignation)"
        Other _      -> Left $ TokenError t "unexpected (un-handled error in configuration file)"

-- Parse a command Line
--
-- expecting:
-- 1. a type
-- 2. a valid FQDN
-- 3. maybe command line flags
-- 3. maybe command line options ( 'key' '=' 'value')
parseCommandLine :: [Token Value]
                 -> Either (TokenError Value) (CommandLine, [Token Value])
parseCommandLine [] = Left $ TokenError (Token 0 0 (Other "<unknown>")) "unexpected EndOfBuffer"
parseCommandLine l = do
    (t, l1) <- parseCommandLineType l
    (_, l1') <- eatMaybeContinueLine l1
    when (null l1') $ Left $ TokenError (head l) "expecting a FQDN after this token"
    (fqdn, l2) <- parseCommandLineFQDN l1'
    (_, l2') <- eatMaybeContinueLine l2
    (others, l3) <- parseCommandLineFlags l2'
    (opts, l4) <- parseCommandLineOptions l3
    Right (CommandLine t fqdn others opts, l4)

-- eat a continue line if present, else do nothing
-- (this function never fail)
eatMaybeContinueLine :: [Token Value]
                     -> Either (TokenError Value) (Maybe (Token Value), [Token Value])
eatMaybeContinueLine [] = Right (Nothing, [])
eatMaybeContinueLine (x:xs) =
    case tokenValue x of
        ContinueLine -> Right (Just x , xs)
        _            -> Right (Nothing, x:xs)

-- attempt to read the command line type
-- (the types are the DNS TYPEs defined in /dns/)
parseCommandLineType :: [Token Value] -> Either (TokenError Value) (Token TYPE, [Token Value])
parseCommandLineType [] = Left $ TokenError (Token 0 0 (Other "")) "expected DNS Type: A, AAAA, NS, MX, PTR, SRV, SOA, CNAME, DNAME, TXT"
parseCommandLineType (t:xs) =
    case tokenValue t of
        Value typestr -> do
            dnstype <- either (Left . TokenError t) Right $ readEither typestr
            Right (tokenToToken t dnstype, xs)
        _ -> Left $ TokenError t "expected DNS Type: A, AAAA, NS, MX, SRV, SOA, CNAME, DNAME, TXT"

-- attempt to read a Valid FQDN
parseCommandLineFQDN :: [Token Value]
                     -> Either (TokenError Value) (Token ValidFQDN, [Token Value])
parseCommandLineFQDN [] = Left $ TokenError (Token 0 0 (Other "")) "expected Valid FQDN"
parseCommandLineFQDN (t:xs) =
    case tokenValue t of
        Value fqdnstr -> do
            fqdn <- either (Left . TokenError t) Right $ validateFQDNEither (BC.pack fqdnstr)
            Right (tokenToToken t fqdn, xs)
        _ -> Left $ TokenError t "expected FQDN"

-- attempt to read the Command's Flags
parseCommandLineFlags :: [Token Value] -> Either (TokenError Value) ([Token String], [Token Value])
parseCommandLineFlags [] = Right ([], [])
parseCommandLineFlags [t] =
    case tokenValue t of
        Value str    -> Right ([tokenToToken t str], [])
        NewLine      -> Right ([], [t])
        CloseScope   -> Right ([], [t])
        ContinueLine -> Left $ TokenError t "unexpected Continue Line '\\'"
        _            -> Left $ TokenError t "expecting a String instead"
parseCommandLineFlags (t1:t2:ts) =
    case (tokenValue t1, tokenValue t2) of
        (NewLine     , _     ) -> Right ([], t1:t2:ts)
        (CloseScope  , _     ) -> Right ([], t1:t2:ts)
        (Value _     , Assign) -> Right ([], t1:t2:ts)
        (ContinueLine, _     ) -> parseCommandLineFlags (t2:ts)
        (Value str   , _     ) -> do
            (others, remains) <- parseCommandLineFlags (t2:ts)
            Right ((tokenToToken t1 str):others, remains)
        _         -> Left $ TokenError t1 "expecting a String instead"

-- attempt to read the Command Line Options
parseCommandLineOptions :: [Token Value] -> Either (TokenError Value) (Opts, [Token Value])
parseCommandLineOptions [] = Right (emptyOpts, [])
parseCommandLineOptions [t] =
    case tokenValue t of
        NewLine    -> Right (emptyOpts, [t])
        CloseScope -> Right (emptyOpts, [t])
        _          -> Left $ TokenError t "not enough data to write a Valide Options"
parseCommandLineOptions [t1,t2] =
    case (tokenValue t1, tokenValue t2) of
        (NewLine   , _     ) -> Right (emptyOpts, [t1,t2])
        (CloseScope, _     ) -> Right (emptyOpts, [t1,t2])
        (Value _   , Assign) -> Left $ TokenError t2 "missing value on assignation"
        _       -> Left $ TokenError t1 "not enough data to write a Valide Options"
parseCommandLineOptions (t1:t2:t3:ts) =
    case (tokenValue t1, tokenValue t2, tokenValue t3) of
        (ContinueLine, NewLine, _      ) -> parseCommandLineOptions (t3:ts)
        (ContinueLine, _      , _      ) -> parseCommandLineOptions (t2:t3:ts)
        (NewLine     , _      , _      ) -> Right (emptyOpts, t1:t2:t3:ts)
        (CloseScope  , _      , _      ) -> Right (emptyOpts, t1:t2:t3:ts)
        (Value k     , Assign , Value v) -> do
            (opts, remains) <- parseCommandLineOptions ts
            Right (insertOpts k (tokenToToken t3 v) opts, remains)
        _       -> Left $ TokenError t1 "not enough data to write a Valide Options"

-- this function is called if the char '@' has been found (i.e. the token /Command/ has been read)
parseTokenCommand :: [Token Value] -> Either (TokenError Value) (CommandScope, [Token Value])
parseTokenCommand []     = Left $ TokenError (Token 0 0 (Other "unexpected error")) "unexpected error"
parseTokenCommand (s:xs) = do
    symbol <- parseTokenCommandSymbol s
    when (null xs) $ Left $ TokenError s "expecting an Open Scope '{' after this point"
    parseTokenCommandOpenScope (head xs)
    parseTokens (tail xs) (CommandScope symbol [] [])

-- read a symbol
parseTokenCommandSymbol :: Token Value -> Either (TokenError Value) (Token String)
parseTokenCommandSymbol t =
    case tokenValue t of
        Value str -> Right $ Token (tokenLine t) (tokenColumn t) str
        _         -> Left $ TokenError t "expecting a Command Symbol"

-- read an OpenScope
parseTokenCommandOpenScope :: (Token Value) -> Either (TokenError Value) ()
parseTokenCommandOpenScope t =
    case tokenValue t of
        OpenScope -> Right ()
        _         -> Left $ TokenError t "expecting a Command Open Scope"

-------------------------------------------------------------------------------
--                                     Lexer                                 --
-------------------------------------------------------------------------------

-- A new line in the Lexing context
-- It is not expected to exported outside of this module and is used internaly
--
-- This type is only use to keep the line number while parsing the file.
data Line = Line Int String
    deriving (Show, Eq)

-- | Parse the lines and returns the list of Token
linesToToken :: [String] -> Either (TokenError Value) [Token Value]
linesToToken l = do
    -- 1. give a line number to every entry we got
    let rawLines = zipWith (\ls v -> Line ls v) [1..] l

    -- 2. split each lines into a list of token
    concat . map filterEmptyLine . map (filter (not . isTokenComment)) <$> mapM splitLineToTokens rawLines
  where
    filterEmptyLine :: [Token Value] -> [Token Value]
    filterEmptyLine [] = []
    filterEmptyLine [t]
        | isTokenNewLine t = []
        | otherwise        = [t]
    filterEmptyLine ts = ts

splitLineToTokens :: Line -> Either (TokenError Value) [Token Value]
splitLineToTokens (Line lineNumber content) = do
    splitLine 1 content
  where
    splitLine :: Int    -- ^ Column
              -> String -- ^ remains of the actual line
              -> Either (TokenError Value) [Token Value]
    splitLine col [] = Right $ [Token lineNumber col NewLine]
    splitLine col (x:xs) =
        case x of
            -- Remove the useless white spaces
            ' '  -> splitLine (col + 1) xs

            -- Remove the useless tabulations
            '\t' -> splitLine (col + 1) xs

            -- This is a comment, put all the remaining string in the Token and return
            '#'  -> Right $ (Token lineNumber col (Comment xs)):[Token lineNumber (col + length xs + 1) NewLine]

            -- Detect we start a command
            '@'  -> (:) (Token lineNumber col (Command))    <$> splitLine (col + 1) xs

            -- Detect start a scope
            '{'  -> (:) (Token lineNumber col (OpenScope))  <$> splitLine (col + 1) xs

            -- Detect end a scope
            '}'  -> (:) (Token lineNumber col (CloseScope)) <$> splitLine (col + 1) xs

            -- Parse the assignation character
            '='  -> (:) (Token lineNumber col (Assign))     <$> splitLine (col + 1) xs

            -- Detect the continue token
            '\\' -> do
                l <- splitLine (col + 1) xs -- make sure there is nothing remaining on this line
                case l of
                    -- This case is already managed my the splitLineToTokens
                    -- in the case of an empty list
                    []  -> Right $ [Token lineNumber col ContinueLine]
                    -- In this case we want the line to continue, so we drop the NewLine
                    [t] | isTokenNewLine t -> Right $ [Token lineNumber col ContinueLine]
                    -- That's something unexpected
                    _   -> Left  $ TokenError (head l) "Unexpecting token after a ContinueLine (expected End Of Line)"

            _ -> case takeValue (x:xs) of
                    Left  err -> Left $ TokenError (Token lineNumber col (Other (x:xs))) err
                    Right (str, readSize) -> do
                        l <- splitLine (col + readSize) (drop readSize (x:xs))
                        Right $ (Token lineNumber col (Value str)):l

-- take both:
-- * a quoted string
-- * a non-quoted string
takeValue :: String
          -> Either String (String, Int)
takeValue [] = Left "unexpected empty value"
takeValue l
    | head l == '"' = do
        str <- takeValueQuoted (tail l)
        Right (str, length str + 2)
    | otherwise     = do
        str <- takeValueUnquoted l
        Right (str, length str)

takeValueUnquoted :: String
                  -> Either String String
takeValueUnquoted [] = Right []
takeValueUnquoted (x:xs)
    | x == '"'              = Left "Unexpected Double Quote '\"' symbole in an Unquoted String Value"
    | x `elem` " \t#@{}\\=" = Right []
    | otherwise             = (:) x <$> takeValueUnquoted xs

takeValueQuoted :: String
                -> Either String String
takeValueQuoted str =
    case str of
        []          -> Left "Unexpected end of file in a Quoted String"
        '"' :_      -> Right [] -- This is the Stop point
        '\\':[]     -> Left "Unexpected escape character in a Quoted String"
        '\\':'"':xs -> ((:) '\\' . (:) '"') <$> takeValueQuoted xs
        x:xs        -> ((:) x) <$> takeValueQuoted xs
