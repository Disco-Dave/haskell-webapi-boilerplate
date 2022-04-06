module Boilerplate.Users.Password (
  Password,
  Error (..),
  fromText,
  PasswordHash (..),
  hash,
  fakeHash,
  matches,
) where

import qualified Data.Password.Argon2 as Argon2
import Data.Text (Text)
import qualified Data.Text as Text

newtype Password = Password Text

data Error
  = IsEmpty
  | IsShorterThan8Characters
  | IsLongerThan500Characters
  deriving (Show, Eq)

fromText :: Text -> Either Error Password
fromText text
  | Text.null text = Left IsEmpty
  | passwordLength < 8 = Left IsShorterThan8Characters
  | passwordLength > 500 = Left IsLongerThan500Characters
  | otherwise = Right $ Password text
 where
  passwordLength = Text.length text

newtype PasswordHash = PasswordHash
  { fromPasswordHash :: Text
  }

hash :: Password -> IO PasswordHash
hash (Password password) = do
  argonHash <- Argon2.hashPassword $ Argon2.mkPassword password
  pure . PasswordHash $ Argon2.unPasswordHash argonHash

fakeHash :: PasswordHash
fakeHash =
  PasswordHash "$argon2id$v=19$m=65536,t=2,p=1$UBYq/20/glODRJg4uv5w5w$UeJVnw9voyh6uNeD35aTJbHgtMnKvg3pOha1UbbXvCY"

matches :: Password -> PasswordHash -> Bool
(Password password) `matches` (PasswordHash passwordHash) =
  let argonPassword = Argon2.mkPassword password
      argonHash = Argon2.PasswordHash passwordHash
   in case Argon2.checkPassword argonPassword argonHash of
        Argon2.PasswordCheckFail -> False
        Argon2.PasswordCheckSuccess -> True
