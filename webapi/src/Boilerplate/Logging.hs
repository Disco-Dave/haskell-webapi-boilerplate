module Boilerplate.Logging (
  LoggingConfig (..),
  LoggingData (..),
  withLoggingData,
) where

import Control.Exception (bracket)
import qualified Katip
import System.IO (stdout)

data LoggingConfig = LoggingConfig
  { minSeverity :: Katip.Severity
  , verbosity :: Katip.Verbosity
  , environment :: Katip.Environment
  , useColor :: Bool
  , disableLogging :: Bool
  }
  deriving (Show, Eq)

data LoggingData = LoggingData
  { namespace :: Katip.Namespace
  , context :: Katip.LogContexts
  , logEnv :: Katip.LogEnv
  }

withLoggingData :: LoggingConfig -> (LoggingData -> IO a) -> IO a
withLoggingData LoggingConfig{..} use =
  let makeLogEnv = do
        logEnv <- Katip.initLogEnv "boilerplate" environment

        if disableLogging
          then pure logEnv
          else do
            scribe <-
              Katip.mkHandleScribeWithFormatter
                Katip.jsonFormat
                (Katip.ColorLog useColor)
                stdout
                (Katip.permitItem minSeverity)
                verbosity

            Katip.registerScribe "stdout" scribe Katip.defaultScribeSettings logEnv
   in bracket makeLogEnv Katip.closeScribes $ \logEnv ->
        let loggingData =
              LoggingData
                { namespace = ""
                , context = mempty
                , logEnv = logEnv
                }
         in use loggingData


