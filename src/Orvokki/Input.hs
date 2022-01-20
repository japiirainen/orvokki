-- | This module contains the functions and types that power to URI-base imports
module Orvokki.Input
    ( -- * Input
      Input(..)
    ) where

import Data.Text (Text)
import Orvokki.Pretty (Pretty(..))
import System.FilePath ((</>))

import qualified System.FilePath as FilePath

{-| Input to the interpreter.
    You should prefer to use `Path` if possible (for better error messages and
    correctly handling transitive imports).  The `Code` constructor is intended
    for cases like interpreting code read from standard input.
-}
data Input
    = Path FilePath
    -- ^ The path to the code
    | Code String Text
    -- ^ Source code: @Code name content@
    deriving (Eq, Show)

instance Semigroup Input where
    _ <> Code name code = Code name code

    Code _ _    <> Path child = Path child
    Path parent <> Path child = Path (FilePath.takeDirectory parent </> child)

instance Pretty Input where
    pretty (Code _ code) = pretty code
    pretty (Path path) = pretty path
