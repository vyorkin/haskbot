module Main (main) where

import qualified Haskbot
import qualified Haskbot.Config as Config

main :: IO ()
main = Config.load >>= Haskbot.run
