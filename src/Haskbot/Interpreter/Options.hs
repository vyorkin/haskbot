module Haskbot.Interpreter.Options
  ( -- * Types
    Options(..)
    -- * Functions
  , defaultOptions
  ) where

-- | Haskell interpreter options.
data Options = Options
  { -- ^ Enable resource limits (using POSIX rlimits).
    modules :: ![String]
    -- ^ Additional modules to import.
  , extensions :: ![String]
    -- ^ Pass additional flags enabling extensions
    -- just like you would to ghc. Example: @-XViewPatterns@.
  } deriving (Show)

-- | Default interpreter options.
defaultOptions :: Options
defaultOptions =
   let modules = []
       extensions = []
    in Options {..}
