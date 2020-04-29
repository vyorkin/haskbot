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
import qualified Data.Text as Text
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<|>))
import Network.HTTP.Client (newManager, noProxy, useProxy, managerSetProxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (ClientEnv(..), mkClientEnv)
import qualified Telegram.Bot.API as Telegram
import Telegram.Bot.API (botBaseUrl)
import Telegram.Bot.Simple (BotApp(..), Eff, BotM, ReplyMessage(..), startBot_, (<#), replyText, reply, toReplyMessage)
import Telegram.Bot.Simple.Debug (traceBotDefault)
import Telegram.Bot.Simple.UpdateParser (parseUpdate, plainText, command)
import qualified Telegram.Bot.API.Methods as ParseMode

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
  -- | Display a help text with usage info.
  | Help
  -- | Search in Hoogle.
  | Hoogle Text
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

usage :: Text
usage = Text.unlines
  [ "Usage:"
  , ""
  , "* /e expr - Evalute <expr>"
  , "* /t expr - Get <expr> type"
  , "* /k Type - Get <Type> kind"
  , "* /h Type - Search <Type> in Hoogle"
  , "* /help - Display this message"
  ]

-- | Processes incoming 'Telegram.Update's and turns them into 'Action's.
updateToAction :: Model -> Telegram.Update -> Maybe Action
updateToAction _ = parseUpdate $
      Help <$ command "help"
  <|> Hoogle <$> command "h"
  <|> Interpreter . TypeOf <$> command "t"
  <|> Interpreter . KindOf <$> command "k"
  <|> Interpreter . Eval   <$> command "e"

-- | Creates a new 'Action' handler.
mkActionHandler :: Options -> Action -> Model -> Eff Action Model
mkActionHandler opts action model = case action of
  Done ->
    pure model
  Help ->
    model <# (replyText usage >> done)
  Hoogle _ ->
    -- TODO
    model <# (replyText "Not implemented yet" >> done)
  Interpreter cmd -> model <# do
    res <- wrapCode <$> interpret cmd
    replyMarkdown res
    done
  where
    interpret :: Command -> BotM Text
    interpret = liftIO . Interpreter.run opts

replyMarkdown :: Text -> BotM ()
replyMarkdown s = reply (toReplyMessage s)
  { replyMessageParseMode = Just ParseMode.Markdown }

wrapCode :: Text -> Text
wrapCode s = "```\n" <> s <> "\n```"

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
