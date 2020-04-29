module Haskbot.Interpreter
  ( -- * Functions
    run'
  , run
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
  (eval, set, reset, setImportsQ, loadModules, liftIO,
   installedModulesInScope, languageExtensions, availableExtensions,
   typeOf, setTopLevelModules, runInterpreter, interpret, infer,
   OptionVal(..), Interpreter, MonadInterpreter,
   InterpreterError(..), GhcError(..),
   Extension(UnknownExtension))
import qualified Language.Haskell.Interpreter as Hint

import Haskbot.Interpreter.Context (defaultModules, defaultQualifiedModules, defaultPackages)
import Haskbot.Resources (limitResources)
import Haskbot.Interpreter.Error (ppInterpreterError)
import Haskbot.Interpreter.Options (Options(..), defaultOptions)

run' :: Options -> Text -> IO Text
run' options expr =
  either ppInterpreterError Text.pack <$> run options expr

setImports :: MonadInterpreter m => Maybe [String] -> m ()
setImports extra = do
  let unqualifiedModules = zip (defaultModules ++ fromMaybe [] extra) (repeat Nothing)
  setImportsQ (unqualifiedModules ++ defaultQualifiedModules)

run
  :: (MonadIO m, MonadMask m)
  => Options
  -> Text
  -> m (Either InterpreterError String)
run Options{..} expr = runInterpreter $ do
  liftIO $ limitResources rLimits
  setImports modules
  eval $ Text.unpack expr
