{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This module contains the implementation of the @grace repl@ subcommand

module Orvokki.REPL
    ( -- * REPL
      repl
    , getWidth
    , defaultWidth
    ) where

import Control.Applicative (empty)
import Control.Exception.Safe (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState(..))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.String.Interpolate (__i)
import Orvokki.Lexer (reserved)
import System.Console.Haskeline (Interrupt(..))
import System.Console.Repline (CompleterStyle(..), MultiLine(..), ReplOpts(..))
import System.Console.Terminal.Size (Window(..))


import qualified Control.Monad as Monad
import qualified Control.Monad.State as State
import qualified Data.Text as Text
import qualified Orvokki.Pretty as Pretty
import qualified System.Console.Haskeline as Repline
import qualified System.Console.Haskeline.Completion as Completion
import qualified System.Console.Repline as Repline
import qualified System.Console.Terminal.Size as Size
import qualified System.IO as IO


getWidth :: IO Int
getWidth = do
    maybeWindow <- Size.size

    let renderWidth =
            case maybeWindow of
                Nothing         -> defaultWidth
                Just Window{..} -> width

    return renderWidth

-- | The default width to use
defaultWidth :: Int
defaultWidth = 80

-- | Entrypoint for the @grace repl@ subcommand
repl :: IO ()
repl = do
    -- let interpret input = do
    --         Right 1 :: Either String Int
    --         -- Except.runExceptT (Interpret.interpretWith context Nothing manager input)

    -- let err e =
    --         liftIO (Text.IO.hPutStrLn IO.stderr (Text.pack (displayException e)))

    let command _ = do
            -- let input = Code "(input)" (Text.pack string)

            -- eitherResult <- interpret input

            -- case eitherResult of
            --     Left e -> do
            --         err e

            --     Right (_inferred, value) -> do
         width <- liftIO getWidth
         liftIO (Pretty.renderIO True width IO.stdout (Pretty.pretty (show (1 :: Int)) <> "\n"))

    let help _string = do
            liftIO (putStrLn [__i|
                Type any expression to normalize it or use one of the following commands:
                :help
                    Print help text and describe options
                :paste
                    Start a multi-line input. Submit with <Ctrl-D>
                :quit
                    Exit the REPL
            |])


    let quit _ =
            liftIO (throwIO Interrupt)

    let options =
            [ ("help", Repline.dontCrash . help)
            , ("let", Repline.dontCrash . \_ -> return ())
            -- `paste` is included here for auto-completion purposes only.
            -- `repline`'s `multilineCommand` logic overrides this no-op.
            , ("paste", Repline.dontCrash . \_ -> return ())
            , ("quit", quit)
            , ("type", Repline.dontCrash . \_ -> return ())
            ]

    let tabComplete =
            Custom (Repline.runMatcher [ (":", completeCommands) ] complete)
          where
            completeCommands =
                Repline.listCompleter (fmap adapt options)
              where
                adapt (c, _) = ":" <> c

            complete =
                foldr Repline.fallbackCompletion Completion.noCompletion
                    [ completeReserved
                    , completeIdentifiers
                    , completeFields
                    ]

            completeReserved =
                Repline.listCompleter (fmap Text.unpack (toList reserved))

            completeIdentifiers args = do
                context <- get

                let completions =
                        fmap (\(name, _, _) -> name) context

                Repline.listCompleter (fmap Text.unpack completions) args

            completeFields =
                Repline.wordCompleter \prefix -> do
                    let toNonEmpty (x : xs) =  x :| xs
                        toNonEmpty      []  = "" :| []

                    let loop (c0 :| c1 : cs) context = do
                            let newContext = do
                                    (name, _) <- context

                                    Monad.guard (c0 == name)

                                    empty
                                    -- case Type.node type_ of
                                    --     Type.Record (Type.Fields keyTypes _) -> do
                                    --         keyTypes
                                    --     _ -> do
                                    --         empty

                            results <- loop (c1 :| cs) newContext

                            let prepend result = c0 <> "." <> result

                            return (fmap prepend results)
                        loop (c0 :| []) context = return do
                            (name, _) <- context

                            Monad.guard (Text.isPrefixOf c0 name)

                            return name

                    let startingComponents =
                            toNonEmpty (Text.splitOn "." (Text.pack prefix))

                    context <- get

                    let startingContext = do
                            (name, type_, _) <- context

                            return (name, type_)

                    results <- loop startingComponents startingContext

                    return (fmap Text.unpack results)

    
    let banner MultiLine  = return "... "
        banner SingleLine = return ">>> "

    let prefix = Just ':'

    let multilineCommand = Just "paste"

    let initialiser = return ()

    let finaliser = return Repline.Exit
    

    let action = Repline.evalReplOpts ReplOpts{..}

    State.evalStateT action []
