module Haskbot.Bot
  ( -- * Types
    Model(..)
  , Action(..)
    -- * Functions
  , mkBot
  , updateToAction
  , mkActionHandler
  , run
  ) where

import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<|>))
import Network.HTTP.Client (newManager, noProxy, useProxy, managerSetProxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (ClientEnv(..), mkClientEnv)
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.API (botBaseUrl)
import Telegram.Bot.Simple (BotApp(..), Eff, BotM, startBot_, (<#), replyText)
import Telegram.Bot.Simple.Debug (traceBotDefault)
import Telegram.Bot.Simple.UpdateParser (parseUpdate, plainText, command, callbackQueryDataRead)

import Haskbot.Config (Config(..))
import Haskbot.Interpreter (Options(..), Command(..))
import qualified Haskbot.Interpreter as Interpreter

-- | Bot conversation state model.
data Model = Model deriving (Show)

emptyModel :: Model
emptyModel = Model

-- | Actions bot can perform.
data Action
  -- | Perform no action.
  = Done
  -- | Run interpreter command.
  | Interpreter Command
  deriving (Show, Read)

-- | Helper function for convenience.
done :: Applicative f => f Action
done = pure Done

-- | Makes a bot application.
mkBot :: Options -> BotApp Model Action
mkBot opts = BotApp
  { botInitialModel = emptyModel
  , botAction = flip updateToAction
  , botHandler = mkActionHandler opts
  , botJobs = []
  }

-- | Processes incoming 'Telegram.Update's and turns them into 'Action's.
updateToAction :: Model -> Telegram.Update -> Maybe Action
updateToAction _ = parseUpdate $
      Interpreter . TypeOf <$> command "t"
  <|> Interpreter . KindOf <$> command "k"
  <|> Interpreter . Eval   <$> plainText
  <|> callbackQueryDataRead

-- | Creates a new 'Action' handler.
mkActionHandler :: Options -> Action -> Model -> Eff Action Model
mkActionHandler opts action model = case action of
  Done -> pure model
  Interpreter cmd -> model <# (interpret cmd >>= replyText >> done)
  where
    interpret :: Command -> BotM Text
    interpret = liftIO . Interpreter.run opts

-- | Creates a new 'ClientEnv' using a given 'Config'.
newClientEnv :: Config -> IO ClientEnv
newClientEnv Config{..} = do
  let proxy = maybe noProxy useProxy configProxy
  let settings = managerSetProxy proxy tlsManagerSettings
  mkClientEnv <$> newManager settings <*> pure (botBaseUrl configBotToken)

-- | Run bot with a given 'Config' and 'Options'.
run :: Config -> Options -> IO ()
run cfg opts = do
  env <- newClientEnv cfg
  let bot = mkBot opts
  startBot_ (traceBotDefault bot) env
