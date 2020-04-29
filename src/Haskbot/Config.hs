{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Haskbot.Config
  ( -- * Types
    Config(..)
  , Interpreter(..)
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
  , configInterpreter :: !Interpreter
  }

-- | Interpreter configuration.
data Interpreter = Interpreter
  { interpreterTimeLimit :: !Int
  -- ^ Time limit for compilation and evaluation.
  , interpreterRLimits :: !Bool
  -- ^ Enable resource limits (using POSIX rlimits).
  }

-- | TOML codec for the 'Config' data type.
configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.diwrap (Toml.text "bot.token") .= configBotToken
    <*> Toml.dioptional (Toml.table proxyCodec "proxy") .= configProxy
    <*> Toml.table interpreterCodec "interpreter" .= configInterpreter

proxyCodec :: TomlCodec Proxy
proxyCodec = Proxy
  <$> Toml.byteString "host" .= proxyHost
  <*> Toml.int "port" .= proxyPort

interpreterCodec :: TomlCodec Interpreter
interpreterCodec = Interpreter
  <$> Toml.int "timeLimit" .= interpreterTimeLimit
  <*> Toml.bool "rLimits" .= interpreterRLimits

-- | Loads the @config.toml@ file.
load :: MonadIO m => m Config
load = Toml.decodeFile configCodec "config.toml"
