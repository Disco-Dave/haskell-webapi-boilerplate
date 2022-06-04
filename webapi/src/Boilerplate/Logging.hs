module Boilerplate.Logging (
  LoggingConfig (..),
  LoggingData (..),
  withLoggingData,
) where

import Control.Exception (bracket)
import qualified Data.Text as Text
import Data.Version (showVersion)
import qualified Katip
import Paths_boilerplate (version)
import System.IO (stdout)


data LoggingConfig = LoggingConfig
  { minSeverity :: Katip.Severity
  , verbosity :: Katip.Verbosity
  , environment :: Katip.Environment
  , useColor :: Bool
  , useBracket :: Bool
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
                (if useBracket then Katip.bracketFormat else Katip.jsonFormat)
                (Katip.ColorLog useColor)
                stdout
                (Katip.permitItem minSeverity)
                verbosity

            Katip.registerScribe "stdout" scribe Katip.defaultScribeSettings logEnv

      initialContext =
        let payload = Katip.sl "version" (Text.pack (showVersion version))
         in Katip.liftPayload payload
   in bracket makeLogEnv Katip.closeScribes $ \logEnv ->
        let loggingData =
              LoggingData
                { namespace = Katip.Namespace []
                , context = initialContext
                , logEnv = logEnv
                }
         in use loggingData
