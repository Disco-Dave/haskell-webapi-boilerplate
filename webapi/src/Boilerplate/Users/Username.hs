module Boilerplate.Users.Username (
  Username,
  Error (..),
  fromText,
) where

import qualified Data.Char as Char
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text

newtype Username = Username
  { fromUsername :: Text
  }
  deriving (Show, Eq, Ord)

data Error
  = IsEmpty
  | IsLongerThan25Characters
  | IsMoreThanLettersOrNumbers
  deriving (Show, Eq)

fromText :: Text -> Either (NonEmpty Error) Username
fromText text =
  let strippedText = Text.strip text
   in if Text.null strippedText
        then Left (IsEmpty :| [])
        else
          let isTooLong
                | Text.length strippedText > 20 = Just IsLongerThan25Characters
                | otherwise = Nothing
              hasInvalidChars
                | Text.any (not . Char.isAlphaNum) strippedText = Just IsMoreThanLettersOrNumbers
                | otherwise = Nothing
           in case catMaybes [isTooLong, hasInvalidChars] of
                (e : es) -> Left $ e :| es
                [] -> Right $ Username strippedText
