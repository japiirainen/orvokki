{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}

module Orvokki.Syntax
    where

import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)

data Syntax s a = Syntax { location :: s, node :: Node s a }
    deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

data Node s a
    = Variable Text Int
    deriving stock (Eq, Foldable, Functor, Generic, Lift, Show, Traversable)

