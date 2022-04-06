module Boilerplate.Users.UserName where

import qualified Data.Char as Char
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as Text

newtype UserName = UserName
  { fromUserName :: Text
  }
  deriving (Show, Eq, Ord)

data Error
  = IsEmpty
  | IsLongerThan30Characters
  | HasCharactersOtherThanLettersOrNumbers
