module Boilerplate.Todos.Task (
  TaskId (..),
  Status (..),
  Task (..),
) where

import Boilerplate.Todos.Task.Description (Description)
import Control.Lens ((.~), (?~))
import qualified Data.Aeson as Aeson
import Data.Function ((&))
import qualified Data.OpenApi as OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import qualified Servant


newtype TaskId = TaskId
  { fromTaskId :: Integer
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Aeson.ToJSON
    , Aeson.FromJSON
    , Servant.FromHttpApiData
    , Servant.ToHttpApiData
    )


instance OpenApi.ToParamSchema TaskId where
  toParamSchema _ = do
    OpenApi.toParamSchema @Integer Proxy
      & OpenApi.title ?~ "TaskId"
      & OpenApi.description ?~ "A unique id for a single task."
      & OpenApi.example ?~ Aeson.toJSON (TaskId 534)


instance OpenApi.ToSchema TaskId where
  declareNamedSchema _ = do
    textNamedSchema <- OpenApi.declareNamedSchema @Integer Proxy
    pure $
      textNamedSchema
        & OpenApi.name ?~ "TaskId"
        & OpenApi.schema .~ OpenApi.toParamSchema @TaskId Proxy


data Status
  = NotStarted
  | Started UTCTime
  | Removed UTCTime
  | Finished UTCTime
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON Status
instance Aeson.FromJSON Status
instance OpenApi.ToSchema Status


data Task = Task
  { taskId :: TaskId
  , description :: Description
  , status :: Status
  }
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON Task
instance OpenApi.ToSchema Task
