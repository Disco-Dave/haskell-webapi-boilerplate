module TestApp.Database (
  withDatabase,
) where

import Boilerplate.Database (DatabaseUrl (DatabaseUrl))
import Control.Exception (Exception, IOException)
import Control.Monad (void)
import Control.Monad.Catch (Handler (Handler))
import qualified Control.Retry as Retry
import qualified Data.ByteString.Char8 as Char8
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (getLast)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple.Options as PostgresOptions
import Database.Postgres.Temp (Accum (Merge))
import qualified Database.Postgres.Temp as PostgresTemp
import System.Environment (getEnvironment)
import qualified System.Process.Typed as Process
import qualified UnliftIO


newtype WebserverUserPassword = WebserverUserPassword Text


migrate :: DatabaseUrl -> WebserverUserPassword -> IO ()
migrate (DatabaseUrl url) (WebserverUserPassword password) = do
  hostEnv <- getEnvironment

  let processEnv =
        hostEnv
          <> [
               ( "BOILERPLATE_MIGRATE_DATABASE_URL"
               , Char8.unpack url
               )
             ,
               ( "BOILERPLATE_MIGRATE_WEBSERVER_USER_PASSWORD"
               , Text.unpack password
               )
             ]
      command = Process.proc "../database/migrate.sh" []
   in void . Process.readProcess_ $ Process.setEnv processEnv command


withDatabase :: (DatabaseUrl -> IO a) -> IO a
withDatabase use = do
  let tempConfig =
        mempty
          { PostgresTemp.connectionOptions =
              mempty{PostgresOptions.user = pure "postgres"}
          , PostgresTemp.initDbConfig =
              Merge $
                mempty
                  { PostgresTemp.commandLine =
                      mempty{PostgresTemp.keyBased = Map.fromList [("--username=", Just "postgres")]}
                  }
          }

  let retryPolicy =
        Retry.limitRetriesByCumulativeDelay 32_000 $
          Retry.exponentialBackoff 1_000

      exceptionHandlers =
        let restartFor :: forall e a. Exception e => a -> Handler IO Bool
            restartFor _ = Handler @_ @_ @e $ \_ -> pure True
         in [ restartFor @PostgresTemp.StartError
            , restartFor @IOException
            ]

  Retry.recovering retryPolicy exceptionHandlers $ \_ -> do
    result <- PostgresTemp.withConfig tempConfig $ \db ->
      let options = PostgresTemp.toConnectionOptions db

          selectOption defaultValue select =
            fromMaybe defaultValue . getLast $ select options

          privelgedUrl =
            let host = selectOption "" PostgresOptions.host
                user = selectOption "" PostgresOptions.user
                dbname = selectOption "" PostgresOptions.dbname
                port = selectOption 0 PostgresOptions.port
             in DatabaseUrl . Char8.pack . mconcat $
                  [ "postgresql://"
                  , dbname
                  , "?"
                  , "host=" <> host
                  , "&"
                  , "user=" <> user
                  , "&"
                  , "port=" <> show port
                  ]

          password = WebserverUserPassword "password"

          appUrl = do
            let appOptions =
                  options
                    { PostgresOptions.password = pure (Text.unpack $ coerce password)
                    , PostgresOptions.user = pure "webserver"
                    }
             in DatabaseUrl $ PostgresOptions.toConnectionString appOptions
       in migrate privelgedUrl password *> use appUrl

    case result of
      Left err -> UnliftIO.throwIO err
      Right value -> pure value
