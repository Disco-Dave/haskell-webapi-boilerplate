{-# LANGUAGE QuasiQuotes #-}

module Boilerplate.HealthChecks.Http (
  GetIndex,
  getIndex,
  GetDatabases,
  getDatabases,
  Api,
  server,
) where

import Boilerplate.App (HttpApp)
import Boilerplate.Database (withPostgresConnection)

import qualified Database.PostgreSQL.Simple as Postgres
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Servant (
  Description,
  GetNoContent,
  HasServer (ServerT),
  NoContent (NoContent),
  Summary,
  type (:<|>) ((:<|>)),
  type (:>),
 )
import qualified Boilerplate.App as App


type GetIndex =
  Summary "Is this API reachable?"
    :> Description "Allows you to check if the API is reachable. Hardcoded to return 204."
    :> GetNoContent


getIndex :: HttpApp NoContent
getIndex =
  pure NoContent


type GetDatabases =
  Summary "Can the API communicate with our database?"
    :> Description "Allows you to check if the API can communicate with the database. Hardcoded to return 204."
    :> "databases"
    :> GetNoContent


getDatabases :: HttpApp NoContent
getDatabases = do
  _ <-
    App.liftApp . withPostgresConnection $ \connection ->
      Postgres.query_ @(Postgres.Only Int) connection [sql| SELECT 1 |]

  pure NoContent


type Api =
  GetIndex :<|> GetDatabases


server :: ServerT Api HttpApp
server =
  getIndex :<|> getDatabases
