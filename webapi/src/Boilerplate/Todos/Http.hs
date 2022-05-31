module Boilerplate.Todos.Http (
  GetIndex,
  getIndex,
  GetTaskId,
  getTaskId,
  TodosPostRequest (..),
  TodosPostErrorResponse (..),
  PostIndex,
  postIndex,
  TodosStatusPatchRequest (..),
  TodosStatusPatchErrorResponse (..),
  PatchStatus,
  patchStatus,
  Api,
  server,
) where

import Boilerplate.App (HttpApp, liftApp)
import qualified Boilerplate.Http.Helpers as HttpHelpers
import qualified Boilerplate.Todos.Database as Database
import Boilerplate.Todos.Task (Task, TaskId)
import Boilerplate.Todos.Task.Description (Description)
import qualified Boilerplate.Todos.Task.Description as Description
import Boilerplate.Todos.Task.NewStatus (NewStatus)
import qualified Boilerplate.Todos.Task.NewStatus as NewStatus
import Control.Lens ((?~))
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.OpenApi as OpenApi
import Data.Text (Text)
import qualified Data.Text as Text
import Env.Generic (Generic)
import Servant hiding (Description)
import qualified Servant


type GetIndex =
  Summary "Get all of the todo tasks."
    :> Servant.Description "Get all of the todo tasks including ones that are finished."
    :> Get '[JSON] [Task]


getIndex :: HttpApp [Task]
getIndex =
  liftApp Database.queryAllTasks


type GetTaskId =
  Summary "Get a task by its task id."
    :> Servant.Description "Get a task by its task id, returning a 404 if it's not found."
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


newtype TodosPostRequest = TodosPostRequest
  { description :: HttpHelpers.ToSchemaOf Description Text
  }
  deriving (Show, Eq, Generic)
instance Aeson.FromJSON TodosPostRequest
instance OpenApi.ToSchema TodosPostRequest


newtype TodosPostErrorResponse = TodosPostErrorResponse
  { description :: Maybe Text
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON TodosPostErrorResponse


instance OpenApi.ToSchema TodosPostErrorResponse where
  declareNamedSchema _ = do
    namedSchema <-
      OpenApi.genericDeclareNamedSchema @TodosPostErrorResponse
        OpenApi.defaultSchemaOptions
        Proxy
    pure $
      namedSchema
        & OpenApi.schema . OpenApi.description
          ?~ "Provides an error messasge explaining why description may have been malformed. Null means there was no issues with description."


type PostIndex =
  Summary "Add new todo task."
    :> Servant.Description "Add new todo task with its status set to not started."
    :> ReqBody '[JSON] TodosPostRequest
    :> UVerb 'POST '[JSON] '[WithStatus 200 TaskId, WithStatus 400 TodosPostErrorResponse]


postIndex :: TodosPostRequest -> HttpApp (Union '[WithStatus 200 TaskId, WithStatus 400 TodosPostErrorResponse])
postIndex request = do
  result <-
    liftApp . runExceptT $ do
      description <- liftEither . Description.fromText $ coerce request
      lift $ Database.insertTask description

  case result of
    Right taskId ->
      respond $ WithStatus @200 taskId
    Left err ->
      let requirements = " Must be between 1 and 500 characters in length." :: Text
          reason =
            case err of
              Description.Empty ->
                "Description may not be empty."
              Description.LongerThan500 ->
                "Description may not be longer than 500 characters in length."
       in respond . WithStatus @400 . TodosPostErrorResponse . Just $
            reason <> requirements


newtype TodosStatusPatchRequest = TodosStatusPatchRequest
  { status :: HttpHelpers.ToSchemaOf NewStatus Text
  }
  deriving (Show, Eq, Generic)
instance Aeson.FromJSON TodosStatusPatchRequest
instance OpenApi.ToSchema TodosStatusPatchRequest


newtype TodosStatusPatchErrorResponse = TodosStatusPatchErrorResponse
  { status :: Maybe Text
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON TodosStatusPatchErrorResponse


instance OpenApi.ToSchema TodosStatusPatchErrorResponse where
  declareNamedSchema _ = do
    namedSchema <-
      OpenApi.genericDeclareNamedSchema @TodosStatusPatchErrorResponse
        OpenApi.defaultSchemaOptions
        Proxy
    pure $
      namedSchema
        & OpenApi.schema . OpenApi.description
          ?~ "Provides an error messasge explaining why status may have been malformed. Null means there was no issues with status."


type PatchStatus =
  Summary "Update the status on a todo task."
    :> Servant.Description "Update the status on a todo task. Valid values are Start, Finish, or Remove."
    :> Capture "id" TaskId
    :> "status"
    :> ReqBody '[JSON] TodosStatusPatchRequest
    :> UVerb 'PATCH '[JSON] '[WithStatus 204 (), WithStatus 400 TodosStatusPatchErrorResponse, WithStatus 404 Text]


patchStatus :: TaskId -> TodosStatusPatchRequest -> HttpApp (Union '[WithStatus 204 (), WithStatus 400 TodosStatusPatchErrorResponse, WithStatus 404 Text])
patchStatus taskId request = do
  result <-
    liftApp $
      let parsedStatus = NewStatus.fromText $ coerce request
       in maybe (pure Nothing) (fmap Just . Database.updateStatus taskId) parsedStatus

  case result of
    Nothing ->
      let requirements =
            " Must be one of the following strings: " <> Text.intercalate ", " (fmap NewStatus.toText [minBound ..]) <> "."
          reason =
            "Unrecognized status."
       in respond . WithStatus @400 . TodosStatusPatchErrorResponse . Just $
            reason <> requirements
    Just (Left Database.TaskNotFoundError) ->
      respond $ WithStatus @404 ("Unable to find todo task." :: Text)
    Just (Right _) ->
      respond $ WithStatus @204 ()


type Api =
  GetIndex
    :<|> GetTaskId
    :<|> PostIndex
    :<|> PatchStatus


server :: ServerT Api HttpApp
server =
  getIndex
    :<|> getTaskId
    :<|> postIndex
    :<|> patchStatus
