module Boilerplate.Users.UsernameSpec (
  GenUsername (..),
  genUsername,
  spec,
) where

import Boilerplate.Users.Username (Username)
import qualified Boilerplate.Users.Username as Username
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Hedgehog ((===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

fromText :: Text -> Either (Set Username.Error) Username
fromText =
  first (Set.fromList . toList) . Username.fromText

genValidText :: Hedgehog.Gen Text
genValidText =
  let validChars =
        Gen.element $
          mconcat
            [ ['a' .. 'z']
            , ['A' .. 'Z']
            , ['0' .. '9']
            ]
   in Gen.text (Range.constantFrom 15 1 30) validChars

data GenUsername = GenUsername
  { originalInput :: Text
  , username :: Username
  }
  deriving (Show, Eq)

genUsername :: Hedgehog.Gen GenUsername
genUsername = do
  text <- genValidText
  case Username.fromText text of
    Left err -> fail $ show err
    Right username -> pure $ GenUsername text username

spec :: Spec
spec =
  describe "fromText" $ do
    it "strips whitespace from the beginning and end" . hedgehog $ do
      GenUsername{..} <- Hedgehog.forAll genUsername

      leftWhitespace <- Hedgehog.forAll $ Gen.text (Range.constantFrom 5 0 10) (Gen.element " \t\n")
      rightWhitespace <- Hedgehog.forAll $ Gen.text (Range.constantFrom 5 0 10) (Gen.element " \t\n")

      actualUsername <-
        let input = leftWhitespace <> originalInput <> rightWhitespace
         in case Username.fromText input of
              Left err -> fail $ show err
              Right u -> pure u

      actualUsername === username
      Username.toText actualUsername === originalInput
