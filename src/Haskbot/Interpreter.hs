module Haskbot.Interpreter
  ( -- * Types
    Command(..)
    -- * Functions
  , setup
  , run
  , run'
    -- * Re-exports
  , module Haskbot.Interpreter.Context
  , module Haskbot.Interpreter.Error
  , module Haskbot.Interpreter.Options
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadMask)

import Language.Haskell.Interpreter
  (set, reset, setImportsQ, loadModules, liftIO,
   installedModulesInScope, languageExtensions, availableExtensions,
   setTopLevelModules, runInterpreter, interpret, infer,
   OptionVal(..), Interpreter, MonadInterpreter,
   InterpreterError(..), GhcError(..),
   Extension(UnknownExtension))
import qualified Language.Haskell.Interpreter as Hint

import Haskbot.Interpreter.Context (defaultModules, defaultQualifiedModules, defaultPackages)
import Haskbot.Interpreter.Error (ppInterpreterError)
import Haskbot.Interpreter.Options (Options(..), defaultOptions)

-- | Interpreter commands.
data Command
  = Eval Text   -- ^ Evaluate expression.
  | TypeOf Text -- ^ Get type of an expression.
  | KindOf Text -- ^ Get expression kind.
  deriving (Show, Read)

setup :: MonadInterpreter m => Options -> m ()
setup Options{..} = do
  setImports modules

run :: Options -> Command -> IO Text
run opts cmd = either ppInterpreterError Text.pack <$> run' opts cmd

run'
  :: (MonadIO m, MonadMask m)
  => Options
  -> Command
  -> m (Either InterpreterError String)
run' opts cmd = runInterpreter do
  setup opts
  case cmd of
    Eval s -> Hint.eval $ Text.unpack s
    TypeOf s -> Hint.typeOf $ Text.unpack s
    KindOf s -> Hint.kindOf $ Text.unpack s

setImports :: MonadInterpreter m => [String] -> m ()
setImports extra = do
  let unqualifiedModules = zip (defaultModules ++ extra) (repeat Nothing)
  setImportsQ (unqualifiedModules ++ defaultQualifiedModules)
