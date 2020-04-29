module Haskbot.Interpreter.Error
  ( ppInterpreterError
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.Interpreter (InterpreterError(..), GhcError(..), errMsg)

-- | Pretty prints a given 'InterpreterError'.
ppInterpreterError :: InterpreterError -> Text
ppInterpreterError (WontCompile es) = Text.intercalate "\n" (ppGhcError <$> es)
-- Other exceptions indicate some problem,
-- so we rethrow them for debugging purposes
ppInterpreterError e = error $ show e

ppGhcError :: GhcError -> Text
ppGhcError = Text.pack . errMsg
