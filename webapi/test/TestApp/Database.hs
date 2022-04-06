module TestApp.Database (
  withDatabase,
) where

import Boilerplate.Database (DatabaseUrl (DatabaseUrl))
import Control.Monad (void)
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

newtype DatabaseSocketPath = DatabaseSocketPath String

migrate :: DatabaseSocketPath -> DatabaseUrl -> WebserverUserPassword -> IO ()
migrate (DatabaseSocketPath socketPath) (DatabaseUrl url) (WebserverUserPassword password) = do
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
             ,
               ( "BOILERPLATE_MIGRATE_DATABASE_SOCKET"
               , socketPath
               )
             ]
      command = Process.proc "../scripts/sqlx.sh" ["migrate", "run"]
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

  result <- PostgresTemp.withConfig tempConfig $ \db ->
    let options = PostgresTemp.toConnectionOptions db

        selectOption defaultValue select =
          fromMaybe defaultValue . getLast $ select options

        socketPath =
          DatabaseSocketPath $ selectOption "" PostgresOptions.host

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
     in migrate socketPath privelgedUrl password *> use appUrl

  case result of
    Left err -> UnliftIO.throwIO err
    Right value -> pure value
