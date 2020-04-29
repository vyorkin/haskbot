module Haskbot.Interpreter.Options
  ( -- * Types
    Options(..)
    -- * Functions
  , defaultOptions
  ) where

-- | Haskell interpreter options.
data Options = Options
  { timeLimit :: !Int
  -- ^ Time limit for compilation and evaluation.
  , rLimits :: !Bool
  -- ^ Enable resource limits (using POSIX rlimits).
  , modules :: !(Maybe [String])
  -- ^ Additional modules to import.
  , extensions :: !(Maybe [String])
  -- ^ Pass additional flags enabling extensions
  -- just like you would to ghc. Example: @-XViewPatterns@.
  } deriving (Show)

-- | Default interpreter options.
defaultOptions :: Options
defaultOptions =
  let timeLimit = 5
      rLimits = True
      modules = Nothing
      extensions = Nothing
   in Options {..}
