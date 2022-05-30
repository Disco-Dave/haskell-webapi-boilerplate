module Boilerplate (
  withAppData,
  run,
  start,
) where

import qualified Boilerplate.Http as Http
import Boilerplate.App (App, AppData (AppData))
import qualified Boilerplate.App as App
import Boilerplate.Config (Config)
import qualified Boilerplate.Config as Config
import qualified Boilerplate.Database as Database
import qualified Boilerplate.Logging as Logging
import Control.Monad.Cont (ContT (ContT, runContT))
import qualified Katip


withAppData :: Config -> (AppData -> IO a) -> IO a
withAppData config =
  runContT $ do
    loggingData <- ContT $ Logging.withLoggingData (Config.logging config)
    postgresConnectionPool <- ContT $ Database.withPostgresConnectionPool (Config.database config)
    pure $ AppData{..}


run :: Config -> App a -> IO a
run config app =
  withAppData config $ \appData ->
    App.runApp appData (Katip.logExceptionM app Katip.EmergencyS)


start :: Config -> IO ()
start config =
  run config $ Http.start (Config.http config)
