module Boilerplate.Todos.Task.Description (
  Description,
  Error (..),
  fromText,
  toText,
) where

import Control.Lens ((?~))
import qualified Data.Aeson as Aeson
import Data.Function ((&))
import qualified Data.OpenApi as OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as Text


newtype Description = Description
  { fromDescription :: Text
  }
  deriving (Show, Eq, Aeson.ToJSON)


data Error
  = Empty
  | LongerThan500
  deriving (Show, Eq)


fromText :: Text -> Either Error Description
fromText text
  | Text.null strippedText = Left Empty
  | Text.length strippedText > 500 = Left LongerThan500
  | otherwise = Right $ Description strippedText
 where
  strippedText = Text.strip text


toText :: Description -> Text
toText =
  fromDescription


instance OpenApi.ToSchema Description where
  declareNamedSchema _ = do
    textNamedSchema <- OpenApi.declareNamedSchema @Text Proxy
    pure $
      textNamedSchema
        & OpenApi.name ?~ "Description"
        & OpenApi.schema . OpenApi.description ?~ "Describes the task that needs to be done. Must be between 1 and 500 characters in length."
        & OpenApi.schema . OpenApi.example ?~ Aeson.toJSON (Description "Clean the garage.")
