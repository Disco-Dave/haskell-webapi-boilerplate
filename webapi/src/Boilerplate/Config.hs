module Boilerplate.Config (
  Config (..),
  fromEnvironment,
) where

import Boilerplate.Database (DatabaseConfig (DatabaseConfig))
import Boilerplate.Http (HttpConfig (HttpConfig))
import Boilerplate.Logging (LoggingConfig (LoggingConfig))
import Control.Monad ((<=<))
import Data.Foldable (find)
import qualified Data.Text as Text
import qualified Env
import qualified Katip


data Config = Config
  { http :: HttpConfig
  , logging :: LoggingConfig
  , database :: DatabaseConfig
  }


parseHttp :: Env.Parser Env.Error HttpConfig
parseHttp =
  Env.prefixed "HTTP_" $
    HttpConfig
      <$> Env.switch "USE_SWAGGER" (Env.help "When set to true enables swagger ui")
      <*> Env.var Env.auto "PORT" (Env.help "Sets the port the http server runs on")


parseLogging :: Env.Parser Env.Error LoggingConfig
parseLogging =
  let minSeverity :: String -> Either Env.Error Katip.Severity
      minSeverity rawValue =
        let value = Text.toCaseFold . Text.strip $ Text.pack rawValue
            is target =
              let targetValue = Text.dropWhileEnd (== 'S') . Text.pack $ show target
               in value == Text.toCaseFold targetValue
         in case find is [Katip.DebugS ..] of
              Just katipSeverity -> Right katipSeverity
              Nothing -> Left (Env.UnreadError "Valid values are debug, info, notice, warning, error, critical, alert, or emergency.")

      verbosity :: String -> Either Env.Error Katip.Verbosity
      verbosity rawValue =
        let value = Text.toCaseFold . Text.strip $ Text.pack rawValue
            is target =
              let targetValue = Text.dropWhileEnd (== 'S') . Text.pack $ show target
               in value == Text.toCaseFold targetValue
         in case find is [Katip.V0 ..] of
              Just katipVerbosity -> Right katipVerbosity
              Nothing -> Left (Env.UnreadError "Valid values are v0, v1, v2, and v3.")
   in Env.prefixed "LOGGING_" $
        LoggingConfig
          <$> Env.var minSeverity "MIN_SEVERITY" (Env.help "Minimum severity required for a log to be written")
          <*> Env.var verbosity "VERBOSITY" (Env.help "The verbosity of the logs (V0,V1,V2,V3)")
          <*> Env.var (Env.nonempty <=< Env.str) "ENVIRONMENT" (Env.help "Name of the environment to describe this instance in the logs")
          <*> Env.switch "USE_COLOR" (Env.help "Enables color for different severities")
          <*> Env.switch "USE_BRACKET" (Env.help "Use bracket syntax instead of json format")
          <*> pure False


parseDatabase :: Env.Parser Env.Error DatabaseConfig
parseDatabase =
  Env.prefixed "DATABASE_" $
    DatabaseConfig
      <$> Env.var (Env.nonempty <=< Env.str) "URL" (Env.help "Postgres Connection URI")
      <*> Env.var Env.auto "POOL_CACHE_TTL" (Env.help "How long to keep an unused connection open for in seconds.")
      <*> Env.var Env.auto "POOL_MAX_RESOURCES" (Env.help "Upper limit of connections that can be open at one time.")


fromEnvironment :: IO Config
fromEnvironment =
  Env.parse (Env.header "boilerplate") . Env.prefixed "BOILERPLATE_" $
    Config
      <$> parseHttp
      <*> parseLogging
      <*> parseDatabase
