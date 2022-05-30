module Boilerplate.Todos.Http (
  Api,
  server,
) where

import Boilerplate.App (HttpApp, liftApp)
import Boilerplate.Todos.Database as Database
import Boilerplate.Todos.Task (Task, TaskId)
import Data.Text (Text)
import Servant


type GetIndex =
  Summary "Get all of the todo tasks."
    :> Description "Get all of the todo tasks including ones that are finished or removed."
    :> Get '[JSON] [Task]


getIndex :: HttpApp [Task]
getIndex =
  liftApp Database.queryAllTasks


type GetTaskId =
  Summary "Get a task by its task id."
    :> Description "Get a task by its task id, returning a 404 if it's not found"
    :> Capture "id" TaskId
    :> UVerb 'GET '[JSON] '[WithStatus 200 Task, WithStatus 404 Text]


getTaskId :: TaskId -> HttpApp (Union '[WithStatus 200 Task, WithStatus 404 Text])
getTaskId taskId = do
  maybeTask <- liftApp $ Database.queryTaskById taskId
  case maybeTask of
    Nothing ->
      respond $ WithStatus @404 ("Unable to find todo task." :: Text)
    Just task ->
      respond $ WithStatus @200 task


type Api =
  GetIndex
    :<|> GetTaskId


server :: ServerT Api HttpApp
server =
  getIndex
    :<|> getTaskId
