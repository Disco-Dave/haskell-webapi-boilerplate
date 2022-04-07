module Boilerplate.Users.Username (
  Username,
  toText,
  Error (..),
  fromText,
) where

import Control.Lens hiding (Empty)
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes)
import qualified Data.OpenApi as OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as Text

newtype Username = Username Text
  deriving (Show, Eq, Ord)

toText :: Username -> Text
toText (Username username) =
  username

data Error
  = Empty
  | LongerThan30Characters
  | ContainsCharactersOtherThanLettersOrNumbers
  deriving (Show, Eq, Ord, Bounded, Enum)

fromText :: Text -> Either (NonEmpty Error) Username
fromText text =
  let strippedText = Text.strip text
      errors =
        let isEmpty
              | Text.null strippedText = Just Empty
              | otherwise = Nothing
            isLongerThan30Characters
              | Text.length strippedText > 30 = Just LongerThan30Characters
              | otherwise = Nothing
            isContainsCharactersOtherThanLettersOrNumbers
              | Text.any (not . Char.isAlphaNum) strippedText = Just ContainsCharactersOtherThanLettersOrNumbers
              | otherwise = Nothing
         in catMaybes
              [ isEmpty
              , isLongerThan30Characters
              , isContainsCharactersOtherThanLettersOrNumbers
              ]
   in case errors of
        [] -> Right $ Username strippedText
        (e : es) -> Left $ e :| es

instance Aeson.ToJSON Username where
  toJSON = Aeson.toJSON . toText
  toEncoding = Aeson.toEncoding . toText

instance Aeson.FromJSON Username where
  parseJSON = Aeson.withText "Username" $ \text ->
    case fromText text of
      Left err -> fail $ show err
      Right username -> pure username

instance OpenApi.ToSchema Username where
  declareNamedSchema _ = do
    textSchema <- OpenApi.declareNamedSchema @Text Proxy
    pure $
      textSchema
        & OpenApi.name ?~ "Username"
        & OpenApi.schema . OpenApi.description ?~ "Unique name for a user. 1 to 30 characters, letters, and numbers."
        & OpenApi.schema . OpenApi.example ?~ Aeson.toJSON (Username "exampleuser")
