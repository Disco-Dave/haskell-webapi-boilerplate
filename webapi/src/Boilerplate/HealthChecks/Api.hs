{-# LANGUAGE QuasiQuotes #-}

module Boilerplate.HealthChecks.Api (
  Api,
  server,
) where

import Boilerplate.App (ApiApp)
import Boilerplate.Database (withConnection)

import qualified Database.PostgreSQL.Simple as Postgres
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Control.Monad.IO.Class (liftIO)
import Servant (
  Description,
  GetNoContent,
  HasServer (ServerT),
  NoContent (NoContent),
  Summary,
  type (:<|>) ((:<|>)),
  type (:>),
 )

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
    let query = [sql| SELECT 1 |]
     in withConnection $ \connection ->
          liftIO $ Postgres.query_ @(Postgres.Only Int) connection query
  pure NoContent

type Api =
  GetIndex :<|> GetDatabases

server :: ServerT Api ApiApp
server =
  getIndex :<|> getDatabases
