{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module Orvokki.Lexer
    ( -- * Lexer
      Token(..)
    , LocatedToken(..)
    , lex
    , reserved
      -- * Misc
    , validRecordLabel
      -- * Errors related to parsing
    , ParseError(..)
    ) where

import Control.Applicative (empty, (<|>))
import Control.Monad.Combinators (many, manyTill)
import Data.HashSet (HashSet)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Scientific (Scientific)
import Data.String.Interpolate (__i)
import Data.Text (Text)
import Data.Void (Void)
import Orvokki.Location (Location(..), Offset(..))
import Prelude hiding (lex)
import Text.Megaparsec (ParseErrorBundle(..), try, (<?>))

import qualified Control.Monad as Monad
import qualified Control.Monad.Combinators as Combinators
import qualified Data.Char as Char
import qualified Data.HashSet as HashSet
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as Error


-- | Short-hand type synonym used by lexing utilities
type Parser = Megaparsec.Parsec Void Text

space :: Parser ()
space = Lexer.space Megaparsec.Char.space1 (Lexer.skipLineComment "#") empty

symbol :: Text -> Parser Text
symbol = Lexer.symbol space

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

parseToken :: Parser Token
parseToken =
  Combinators.choice
    [ label

    , Combinators.choice
        [ Plus <$ symbol "+"
        , Times <$ symbol "*"
        ] <?> "operator"

    , OpenAngle        <$ symbol "<"
    , CloseAngle       <$ symbol ">"
    , OpenBrace        <$ symbol "{"
    , CloseBrace       <$ symbol "}"
    , OpenBracket      <$ symbol "["
    , CloseBracket     <$ symbol "]"
    , OpenParenthesis  <$ symbol "("
    , CloseParenthesis <$ symbol ")"

    , number
    , text
    ]

parseLocatedToken :: Parser LocatedToken
parseLocatedToken = do
    start <- fmap Offset Megaparsec.getOffset
    token <- parseToken
    return LocatedToken{..}

parseLocatedTokens :: Parser [LocatedToken]
parseLocatedTokens = do
    space
    manyTill parseLocatedToken Megaparsec.eof

lex :: String
    -- ^ Name of the input (used for error messages)
    -> Text
    -- ^ Source code
    -> Either ParseError [LocatedToken]
lex name code =
    case Megaparsec.parse parseLocatedTokens name code of
        Left ParseErrorBundle{..} -> do
            let bundleError :| _ = bundleErrors

            let offset = Offset (Error.errorOffset bundleError)

            Left (LexingFailed (Location{..}))
        Right tokens -> do
            return tokens

number :: Parser Token
number = do
    scientific <- lexeme Lexer.scientific

    case Scientific.toBoundedInteger scientific of
        Nothing  -> return (RealLiteral scientific)
        Just int -> return (Int int)

text :: Parser Token
text = lexeme do
    "\""

    let isText c =
                ('\x20' <= c && c <=     '\x21')
            ||  ('\x23' <= c && c <=     '\x5b')
            ||  ('\x5d' <= c && c <= '\x10FFFF')

    let unescaped = Megaparsec.takeWhile1P (Just "text character") isText

    let unicodeEscape = do
            "\\u"

            codepoint <- Combinators.count 4 Megaparsec.Char.hexDigitChar

            case Read.hexadecimal (Text.pack codepoint) of
                Right (n, "") -> do
                    return (Text.singleton (Char.chr n))
                _             -> do
                    fail [__i|
                    Internal error - invalid unicode escape sequence
                    |]

    let escaped =
            Combinators.choice
                [ "\"" <$ "\\\""
                , "\\" <$ "\\\\"
                , "/"  <$ "\\/"
                , "\b" <$ "\\b"
                , "\f" <$ "\\f"
                , "\n" <$ "\\n"
                , "\r" <$ "\\r"
                , "\t" <$ "\\t"
                , unicodeEscape
                ] <?> "escape sequence"

    texts <- many (unescaped <|> escaped)

    "\""

    return (TextLiteral (Text.concat texts))

-- | Reserved tokens, which can't be used for labels unless they are quoted
reserved :: HashSet Text
reserved =
    HashSet.fromList
      []

isLabel :: Char -> Bool
isLabel c = Char.isAlphaNum c || c == '_' || c == '-' || c == '/'

-- | Returns `True` if the given label is valid
validRecordLabel :: Text -> Bool
validRecordLabel text_  =
    case Text.uncons text_ of
        Nothing     -> False
        Just (h, t) ->
                (Char.isAlpha h || h == '_')
            &&  Text.all isLabel t
            &&  not (HashSet.member text_ reserved)

label :: Parser Token
label = (lexeme . try) do
    let isLabel0 c = Char.isLower c || c == '_'
    c0 <- Megaparsec.satisfy isLabel0 <?> "label character"
    cs <- Megaparsec.takeWhileP (Just "label character") isLabel
    let result = Text.cons c0 cs
    Monad.guard (not (HashSet.member result reserved))
    return (Label result)

data Token
  = Equals
  | GreaterThan
  | LessThan
  | Plus
  | Times
  | Dash
  | OpenAngle
  | CloseAngle
  | OpenBrace
  | CloseBrace
  | OpenBracket
  | CloseBracket
  | OpenParenthesis
  | CloseParenthesis
  | Label Text
  | TextLiteral Text
  | Int Int
  | RealLiteral Scientific
  deriving stock (Eq, Show)

-- | A token with offset information attached, used for reporting line and
--    column numbers in error messages
data LocatedToken = LocatedToken {token :: Token, start :: Offset}
  deriving (Show)

-- | Errors related to lexing and parsing
data ParseError
  = LexingFailed Location
  | ParsingFailed Location
  deriving (Eq, Show)
