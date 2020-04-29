module Haskbot.Bot
  ( -- * Types
    Model(..)
  , Action(..)
    -- * Functions
  , bot
  , handleUpdate
  , handleAction
  , run
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadMask)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client (newManager, noProxy, useProxy, managerSetProxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (ClientEnv(..), mkClientEnv)
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.API (botBaseUrl)
import Telegram.Bot.Simple (BotApp(..), Eff, startBot_, (<#), replyText)
import Telegram.Bot.Simple.Debug (traceBotDefault)
import Telegram.Bot.Simple.UpdateParser (parseUpdate, text)

import Haskbot.Config (Config(..))
import Haskbot.Interpreter (defaultOptions)
import qualified Haskbot.Interpreter as Interpreter

-- | Bot conversation state model.
data Model = Model
  deriving (Show)

-- | Actions bot can perform.
data Action
  = NoAction   -- ^ Perform no action.
  | Eval Text -- ^ Reply some text.
  deriving (Show)

-- | Bot application.
bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

-- | Processes incoming 'Telegram.Update's and turns them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate
  (Eval <$> text)

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoAction -> pure model
  Eval msg -> model <# do
    res <- liftIO $ Interpreter.run' defaultOptions msg
    replyText res
    pure NoAction

-- | Creates a new 'ClientEnv' using a given 'Config'.
newClientEnv :: Config -> IO ClientEnv
newClientEnv Config{..} = do
  let proxy = maybe noProxy useProxy configProxy
  let settings = managerSetProxy proxy tlsManagerSettings
  mkClientEnv <$> newManager settings <*> pure (botBaseUrl configBotToken)

-- | Run bot with a given 'Config'.
run :: Config -> IO ()
run config = do
  env <- newClientEnv config
  startBot_ (traceBotDefault bot) env
