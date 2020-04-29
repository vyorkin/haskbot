module Haskbot.Interpreter
  ( -- * Functions
    eval
  , eval'
  , typeOf
  , typeOf'
    -- * Re-exports
  , module Haskbot.Interpreter.Context
  , module Haskbot.Interpreter.Error
  , module Haskbot.Interpreter.Options
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO, liftIO)
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
import Haskbot.Resources (limitResources)
import Haskbot.Interpreter.Error (ppInterpreterError)
import Haskbot.Interpreter.Options (Options(..), defaultOptions)

setup :: MonadInterpreter m => Options -> m ()
setup Options{..} = do
  liftIO $ limitResources rLimits
  setImports modules

typeOf :: Options -> Text -> IO Text
typeOf options expr =
  either ppInterpreterError Text.pack <$> typeOf' options expr

typeOf'
  :: (MonadIO m, MonadMask m)
  => Options
  -> Text
  -> m (Either InterpreterError String)
typeOf' opts expr = runInterpreter do
  setup opts
  Hint.typeOf $ Text.unpack expr

eval :: Options -> Text -> IO Text
eval options expr =
  either ppInterpreterError Text.pack <$> eval' options expr

eval'
  :: (MonadIO m, MonadMask m)
  => Options
  -> Text
  -> m (Either InterpreterError String)
eval' opts expr = runInterpreter do
  setup opts
  Hint.eval $ Text.unpack expr

setImports :: MonadInterpreter m => Maybe [String] -> m ()
setImports extra = do
  let unqualifiedModules = zip (defaultModules ++ fromMaybe [] extra) (repeat Nothing)
  setImportsQ (unqualifiedModules ++ defaultQualifiedModules)
