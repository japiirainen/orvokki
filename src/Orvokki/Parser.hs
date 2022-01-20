{-# LANGUAGE RecordWildCards #-}
module Orvokki.Parser
    where

import Control.Applicative (many, optional, some, (<|>))
import Control.Applicative.Combinators (endBy, sepBy)
import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Data.Functor (void, ($>))
import Data.List.NonEmpty (NonEmpty(..), some1)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Orvokki.Input (Input(..))
import Orvokki.Lexer (LocatedToken(LocatedToken), ParseError(..), Token)
import Orvokki.Location (Location(..), Offset(..))
import Orvokki.Syntax (Syntax(..))
import Text.Earley (Grammar, Prod, Report(..), rule, (<?>))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Orvokki.Lexer as Lexer
import qualified Text.Earley as Earley
import qualified Text.URI as URI

type Parser r = Prod r Text LocatedToken

grammar :: Grammar r (Parser r (Syntax Offset Input))
grammar = undefined

-- | Parse a complete expression
parse
    :: String
    -- ^ Name of the input (used for error messages)
    -> Text
    -- ^ Source code
    -> Either ParseError (Syntax Offset Input)
parse name code = do
    tokens <- Lexer.lex name code

    case Earley.fullParses (Earley.parser grammar) tokens of
        ([], Report{..}) -> do
            let offset =
                    case unconsumed of
                        []                -> Offset (Text.length code)
                        locatedToken_ : _ -> Lexer.start locatedToken_

            Left (ParsingFailed (Location{..}))

        (result : _, _) -> do
            return result
