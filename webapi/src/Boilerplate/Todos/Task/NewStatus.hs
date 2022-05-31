module Boilerplate.Todos.Task.NewStatus (
  NewStatus (..),
  toText,
  fromText,
) where

import Control.Lens ((?~))
import qualified Data.Aeson as Aeson
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.OpenApi as OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as Text


data NewStatus
  = Start
  | Finish
  | Remove
  deriving (Show, Eq, Enum, Bounded)


toText :: NewStatus -> Text
toText newStatus =
  case newStatus of
    Start -> "start"
    Finish -> "finish"
    Remove -> "remove"


fromText :: Text -> Maybe NewStatus
fromText text =
  let cleanedText =
        Text.toCaseFold $ Text.strip text

      is target =
        let expectedText = Text.toCaseFold $ toText target
         in cleanedText == expectedText
   in List.find is [minBound ..]


instance Aeson.ToJSON NewStatus where
  toJSON = Aeson.toJSON . toText
  toEncoding = Aeson.toEncoding . toText


instance Aeson.FromJSON NewStatus where
  parseJSON = Aeson.withText "NewStatus" $ \text ->
    case fromText text of
      Nothing -> fail "Unrecognized NewStatus."
      Just newStatus -> pure newStatus


instance OpenApi.ToSchema NewStatus where
  declareNamedSchema _ = do
    textSchema <- OpenApi.declareSchema @Text Proxy

    pure . OpenApi.NamedSchema (Just "NewStatus") $
      textSchema
        & OpenApi.description ?~ "A status you may update a todo task to."
        & OpenApi.example ?~ Aeson.toJSON Start
        & OpenApi.enum_ ?~ fmap Aeson.toJSON [minBound :: NewStatus ..]
