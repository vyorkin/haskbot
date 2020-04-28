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

import Data.Text (Text)
import Network.HTTP.Client (newManager, noProxy, useProxy, managerSetProxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (ClientEnv(..), mkClientEnv)
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.API (botBaseUrl)
import Telegram.Bot.Simple (BotApp(..), Eff, startBot_, (<#), replyText)
import Telegram.Bot.Simple.Debug (traceBotDefault)

import Haskbot.Config (Config(..))

-- | Bot conversation state model.
data Model = Model
  deriving (Show)

-- | Actions bot can perform.
data Action
  = NoAction   -- ^ Perform no action.
  | Reply Text -- ^ Reply some text.
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
handleUpdate _ _ = Just (Reply "Got it")

-- | Hot to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoAction -> pure model
  Reply msg -> model <# do
    replyText msg
    pure NoAction

-- | Creates a
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
