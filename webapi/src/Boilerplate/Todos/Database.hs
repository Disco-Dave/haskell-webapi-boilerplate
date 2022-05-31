{-# LANGUAGE QuasiQuotes #-}

module Boilerplate.Todos.Database (
  InvalidDescription (..),
  queryAllTasks,
  queryTaskById,
  TaskNotFoundError (..),
  insertTask,
  updateStatus,
) where

import Boilerplate.App (App (..))
import Boilerplate.Database (withPostgresConnection)
import Boilerplate.Todos.Task (Task (Task), TaskId (TaskId))
import qualified Boilerplate.Todos.Task as Task
import Boilerplate.Todos.Task.Description (Description)
import qualified Boilerplate.Todos.Task.Description as Description
import Boilerplate.Todos.Task.NewStatus (NewStatus)
import qualified Boilerplate.Todos.Task.NewStatus as NewStatus
import Control.Exception (Exception)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Database.PostgreSQL.Simple.FromRow as FromRow
import Database.PostgreSQL.Simple.SqlQQ (sql)
import UnliftIO.Exception (throwIO)


data InvalidDescription = InvalidDescription
  { originalDescription :: Text
  , descriptionError :: Description.Error
  }
  deriving (Show, Eq)
instance Exception InvalidDescription


newtype UnrecognizedStatus = UnrecognizedStatus
  { originalStatusId :: Text
  }
  deriving (Show, Eq)
instance Exception UnrecognizedStatus


data TaskRow = TaskRow
  { taskId :: Integer
  , description :: Text
  , statusId :: Maybe Text
  , statusTimestamp :: Maybe UTCTime
  }
  deriving (Show, Eq)


parseDescription :: Text -> App Description
parseDescription text =
  case Description.fromText text of
    Right parsedDescription ->
      pure parsedDescription
    Left err ->
      throwIO $
        InvalidDescription
          { originalDescription = text
          , descriptionError = err
          }


parseStatusId :: Text -> App (UTCTime -> Task.Status)
parseStatusId rawStatusId
  | is "s" = pure Task.Started
  | is "f" = pure Task.Finished
  | is "r" = pure Task.Removed
  | otherwise = throwIO $ UnrecognizedStatus rawStatusId
 where
  statusId = Text.toCaseFold $ Text.strip rawStatusId
  is target = statusId == Text.toCaseFold (Text.strip target)


parseStatus :: Maybe Text -> Maybe UTCTime -> App Task.Status
parseStatus maybeStatusId maybeTimestamp =
  case (maybeStatusId, maybeTimestamp) of
    (Just rawStatusId, Just timestamp) ->
      ($ timestamp) <$> parseStatusId rawStatusId
    _ ->
      pure Task.NotStarted


rowToTask :: TaskRow -> App Task
rowToTask row = do
  description <-
    parseDescription $ description row

  status <-
    parseStatus
      (statusId row)
      (statusTimestamp row)

  pure $
    Task
      { taskId = Task.TaskId $ taskId row
      , description = description
      , status = status
      }


queryAllTasks :: App [Task]
queryAllTasks = do
  let query =
        [sql|
          SELECT
            task.task_id
            ,task.description
            ,task.task_status_id
            ,task.updated_at
          FROM public.tasks AS task
          WHERE COALESCE(task.task_status_id, 'x') <> 'r';
        |]

      rowParser = do
        taskId <- FromRow.field
        description <- FromRow.field
        statusId <- FromRow.field
        statusTimestamp <- FromRow.field
        pure $ TaskRow{..}

  rows <- withPostgresConnection $ \connection ->
    Postgres.queryWith_ rowParser connection query

  traverse rowToTask rows


queryTaskById :: Task.TaskId -> App (Maybe Task)
queryTaskById taskId = do
  let query =
        [sql|
          SELECT
            task.description
            ,task.task_status_id
            ,task.updated_at
          FROM public.tasks AS task
          WHERE task.task_id = ?
            AND COALESCE(task.task_status_id, 'x') <> 'r';
        |]

      rowParser = do
        description <- FromRow.field
        statusId <- FromRow.field
        statusTimestamp <- FromRow.field
        pure $
          TaskRow
            { taskId = Task.fromTaskId taskId
            , description = description
            , statusId = statusId
            , statusTimestamp = statusTimestamp
            }

      params =
        Postgres.Only (Task.fromTaskId taskId)

  rows <- withPostgresConnection $ \connection ->
    Postgres.queryWith rowParser connection query params

  case rows of
    (row : _) ->
      Just <$> rowToTask row
    _ ->
      pure Nothing


insertTask :: Description -> App TaskId
insertTask description = do
  let query =
        [sql|
          INSERT into public.tasks (description)
          VALUES (?)
          RETURNING task_id;
         |]

      params = Postgres.Only $ Description.toText description

  [Postgres.Only taskId] <-
    withPostgresConnection $ \connection ->
      Postgres.query connection query params

  pure $ TaskId taskId


data TaskNotFoundError = TaskNotFoundError
  deriving (Show, Eq)


updateStatus :: TaskId -> NewStatus -> App (Either TaskNotFoundError ())
updateStatus taskId newStatus = do
  let query =
        [sql|
            UPDATE public.tasks
            SET 
              task_status_id = ?
              ,updated_at = 
                  CASE
                    WHEN task_status_id <> ? THEN CURRENT_TIMESTAMP
                    ELSE updated_at
                  END
            WHERE task_id = ?
              AND COALESCE(task_status_id, 'x') <> 'r'
            RETURNING task_id;
        |]

      params =
        let statusParam =
              case newStatus of
                NewStatus.Start -> "s" :: Text
                NewStatus.Finish -> "f"
                NewStatus.Remove -> "r"
         in ( statusParam
            , statusParam
            , Task.fromTaskId taskId
            )

  rows <- withPostgresConnection $ \connection ->
    Postgres.query @_ @(Postgres.Only Integer) connection query params

  pure $
    if null rows
      then Left TaskNotFoundError
      else Right ()
