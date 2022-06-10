module Boilerplate (
  withAppData,
  runAppWith,
  runApp,
  start,
) where

import Boilerplate.App (App, AppData (AppData))
import qualified Boilerplate.App as App
import Boilerplate.Config (Config)
import qualified Boilerplate.Config as Config
import qualified Boilerplate.Database as Database
import qualified Boilerplate.Http as Http
import qualified Boilerplate.Logging as Logging
import Control.Monad.Cont (ContT (ContT, runContT))
import qualified Katip


withAppData :: Config -> (AppData -> IO a) -> IO a
withAppData config =
  runContT $ do
    loggingData <- ContT $ Logging.withLoggingData (Config.logging config)
    postgresConnectionPool <- ContT $ Database.withPostgresConnectionPool (Config.database config)

    pure $ AppData{..}


runAppWith :: Config -> App a -> IO a
runAppWith config app =
  withAppData config $ \appData ->
    App.runApp appData (Katip.logExceptionM app Katip.EmergencyS)


runApp :: App a -> IO a
runApp app = do
  config <- Config.fromEnvironment
  runAppWith config app


start :: Config -> IO ()
start config =
  runAppWith config $ Http.start (Config.http config)
