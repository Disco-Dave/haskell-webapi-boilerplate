module Boilerplate.Users.PasswordSpec (
  GenPassword (..),
  genPassword,
  spec,
) where

import Boilerplate.Users.Password (Error (..), Password)
import qualified Boilerplate.Users.Password as Password
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.Text (Text)
import qualified Data.Text as Text
import Hedgehog ((===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

fromText :: Text -> Either Error ()
fromText =
  void . Password.fromText

genValidText :: Hedgehog.Gen Text
genValidText =
  Gen.text (Range.constantFrom 46 8 500) Gen.unicode

data GenPassword = GenPassword
  { originalInput :: Text
  , password :: Password
  }

instance Show GenPassword where
  show = Text.unpack . originalInput

genPassword :: Hedgehog.Gen GenPassword
genPassword = do
  text <- genValidText
  case Password.fromText text of
    Left err -> fail $ show err
    Right password -> pure $ GenPassword text password

isValidPassword :: Hedgehog.Gen Text -> Hedgehog.PropertyT IO ()
isValidPassword genText = do
  text <- Hedgehog.forAll genText
  fromText text === Right ()

genTooShortText :: Hedgehog.Gen Text
genTooShortText =
  Gen.text (Range.constantFrom 3 1 7) Gen.unicode

genTooLongText :: Hedgehog.Gen Text
genTooLongText =
  Gen.text (Range.constantFrom 750 501 1000) Gen.unicode

isInvalidPassword :: Password.Error -> Hedgehog.Gen Text -> Hedgehog.PropertyT IO ()
isInvalidPassword expectedError genText = do
  text <- Hedgehog.forAll genText
  fromText text === Left expectedError

spec :: Spec
spec = do
  describe "fromText" $ do
    it "allows all strings from 8 to 500 characters in length" . hedgehog $
      isValidPassword genValidText

    it "disallows all strings from 1 to 7 characters in length" . hedgehog $
      isInvalidPassword Password.IsShorterThan8Characters genTooShortText

    it "disallows all strings from 501 and up in length" . hedgehog $
      isInvalidPassword Password.IsLongerThan500Characters genTooLongText

    it "disallows empty strings" $
      fromText "" `shouldBe` Left Password.IsEmpty

  describe "hashing" . modifyMaxSuccess (const 20) $ do
    it "all passwords can be hashed and verified against their hash" . hedgehog $ do
      GenPassword{password} <- Hedgehog.forAll genPassword
      hash <- liftIO $ Password.hash password
      Password.matches password hash === True

    it "passwords that are different from the one used to generate the hash cannot be verified" . hedgehog $ do
      (firstPassword, secondPassword) <- Hedgehog.forAll . Gen.filter (uncurry $ on (/=) originalInput) $ do
        firstPassword <- genPassword
        secondPassword <- genPassword
        pure (firstPassword, secondPassword)
      hash <- liftIO $ Password.hash (password firstPassword)
      Password.matches (password secondPassword) hash === False
