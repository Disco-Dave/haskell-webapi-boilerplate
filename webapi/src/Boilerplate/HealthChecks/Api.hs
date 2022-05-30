{-# LANGUAGE QuasiQuotes #-}

module Boilerplate.HealthChecks.Api (
  Api,
  server,
) where

import Boilerplate.App (ApiApp)
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
import Data.Text (Text)


type GetIndex =
  Summary "Is this API reachable?"
    :> Description "Allows you to check if the API is reachable. Hardcoded to return 204."
    :> GetNoContent


getIndex :: ApiApp NoContent
getIndex =
  pure NoContent


type GetDatabases =
  Summary "Can the API communicate with our database?"
    :> Description "Allows you to check if the API can communicate with the database. Hardcoded to return 204."
    :> "databases"
    :> GetNoContent


getDatabases :: ApiApp NoContent
getDatabases = do
  _ <-
    withPostgresConnection $ \connection ->
      Postgres.query_ @(Postgres.Only Text) connection 
        [sql| 
          SELECT note 
          FROM public.examples;
        |]

  pure NoContent


type Api =
  GetIndex :<|> GetDatabases


server :: ServerT Api ApiApp
server =
  getIndex :<|> getDatabases
