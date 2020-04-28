{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Haskbot.Config
  ( -- * Types
    Config(..)
    -- * Functions
  , load
  ) where

import Control.Monad.IO.Class (MonadIO)
import Toml (TomlCodec, (.=))
import qualified Toml
import qualified Telegram.Bot.API as Telegram
import Network.HTTP.Client (Proxy(..))

-- | Bot configuration.
data Config = Config
  { configBotToken :: !Telegram.Token
  , configProxy :: !(Maybe Proxy)
  }

-- | TOML codec for the 'Config' data type.
codec :: TomlCodec Config
codec =
  Config
    <$> Toml.diwrap (Toml.text "bot.token") .= configBotToken
    <*> Toml.dioptional (Toml.table proxyCodec "proxy") .= configProxy

proxyCodec :: TomlCodec Proxy
proxyCodec = Proxy
  <$> Toml.byteString "host" .= proxyHost
  <*> Toml.int "port" .= proxyPort

-- | Loads the @config.toml@ file.
load :: MonadIO m => m Config
load = Toml.decodeFile codec "config.toml"
