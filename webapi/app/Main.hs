module Main (main) where

import qualified Boilerplate
import qualified Boilerplate.Config as Config

main :: IO ()
main = do
  config <- Config.fromEnvironment
  Boilerplate.start config
