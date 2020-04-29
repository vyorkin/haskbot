module Main (main) where

import qualified Haskbot
import Haskbot.Config (Config(..), Interpreter(..))
import qualified Haskbot.Config as Config

import Haskbot.Interpreter (defaultOptions)
import Haskbot.Resources (limitResources)

main :: IO ()
main = do
  cfg@Config{ configInterpreter = Interpreter{..} } <- Config.load
  limitResources interpreterRLimits
  Haskbot.run cfg defaultOptions
