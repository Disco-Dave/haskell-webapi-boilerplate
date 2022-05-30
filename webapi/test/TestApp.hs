module TestApp (
  TestApp (..),
  withTestApp,
) where

import qualified Boilerplate
import Boilerplate.App (AppData)
import qualified Boilerplate.App as App
import qualified Boilerplate.Config as Config
import qualified Boilerplate.Database as DatabaseConfig (DatabaseConfig (..))
import qualified Boilerplate.Http as Http
import qualified Boilerplate.Logging as LoggingConfig (LoggingConfig (..))
import Control.Monad.Cont (ContT (ContT, runContT))
import Control.Monad.IO.Class (liftIO)
import qualified Network.Wai.Handler.Warp as Warp
import qualified TestApp.Database as Database


data TestApp = TestApp
  { appData :: AppData
  , port :: Warp.Port
  }


withAppData :: (AppData -> IO a) -> IO a
withAppData =
  runContT $ do
    tempDatabaseUrl <- ContT Database.withDatabase

    config <- liftIO Config.fromEnvironment

    let databaseConfig =
          (Config.database config)
            { DatabaseConfig.url = tempDatabaseUrl
            , DatabaseConfig.numberOfStripes = 1
            , DatabaseConfig.unusedConnectionTimeout = 1
            , DatabaseConfig.maxConnectionsPerStripe = 1
            }
        loggingConfig =
          (Config.logging config)
            { LoggingConfig.disableLogging = True
            }

    ContT . Boilerplate.withAppData $
      config
        { Config.database = databaseConfig
        , Config.logging = loggingConfig
        }


withTestApp :: (TestApp -> IO a) -> IO a
withTestApp = do
  runContT $ do
    appData <- ContT withAppData

    port <-
      let waiApp = Http.makeWaiApplication False
       in ContT $ Warp.testWithApplication (App.runApp appData waiApp)

    pure $ TestApp{..}
