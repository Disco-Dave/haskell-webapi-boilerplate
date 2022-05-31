{-# LANGUAGE DeriveFunctor #-}

module Boilerplate.Http.Helpers (
  ToSchemaOf (..),
  ToParamSchemaOf (..),
) where

import qualified Data.Aeson as Aeson
import qualified Data.OpenApi as OpenApi
import Data.Typeable (Proxy (Proxy), Typeable)
import qualified Servant


newtype ToSchemaOf schema a = ToSchemaOf
  { fromToSchemaOf :: a
  }
  deriving (Show, Eq, Functor)


instance Applicative (ToSchemaOf schema) where
  pure = ToSchemaOf
  (ToSchemaOf f) <*> (ToSchemaOf v) = ToSchemaOf $ f v


instance (Typeable a, OpenApi.ToSchema schema) => OpenApi.ToSchema (ToSchemaOf schema a) where
  declareNamedSchema _ = OpenApi.declareNamedSchema @schema Proxy


instance Aeson.ToJSON a => Aeson.ToJSON (ToSchemaOf _schema a) where
  toJSON = Aeson.toJSON . fromToSchemaOf
  toEncoding = Aeson.toEncoding . fromToSchemaOf


instance Aeson.FromJSON a => Aeson.FromJSON (ToSchemaOf _schema a) where
  parseJSON = fmap ToSchemaOf . Aeson.parseJSON


instance Servant.ToHttpApiData a => Servant.ToHttpApiData (ToSchemaOf _schema a) where
  toUrlPiece = Servant.toUrlPiece . fromToSchemaOf
  toEncodedUrlPiece = Servant.toEncodedUrlPiece . fromToSchemaOf
  toHeader = Servant.toHeader . fromToSchemaOf
  toQueryParam = Servant.toQueryParam . fromToSchemaOf


instance Servant.FromHttpApiData a => Servant.FromHttpApiData (ToSchemaOf _schema a) where
  parseUrlPiece = fmap ToSchemaOf . Servant.parseUrlPiece
  parseHeader = fmap ToSchemaOf . Servant.parseHeader
  parseQueryParam = fmap ToSchemaOf . Servant.parseQueryParam


newtype ToParamSchemaOf schema a = ToParamSchemaOf
  { fromToParamSchemaOf :: a
  }
  deriving (Show, Eq, Functor)


instance Applicative (ToParamSchemaOf schema) where
  pure = ToParamSchemaOf
  (ToParamSchemaOf f) <*> (ToParamSchemaOf v) = ToParamSchemaOf $ f v


instance OpenApi.ToParamSchema schema => OpenApi.ToParamSchema (ToSchemaOf schema a) where
  toParamSchema _ = OpenApi.toParamSchema @schema Proxy


instance Aeson.ToJSON a => Aeson.ToJSON (ToParamSchemaOf _schema a) where
  toJSON = Aeson.toJSON . fromToParamSchemaOf
  toEncoding = Aeson.toEncoding . fromToParamSchemaOf


instance Aeson.FromJSON a => Aeson.FromJSON (ToParamSchemaOf _schema a) where
  parseJSON = fmap ToParamSchemaOf . Aeson.parseJSON


instance Servant.ToHttpApiData a => Servant.ToHttpApiData (ToParamSchemaOf _schema a) where
  toUrlPiece = Servant.toUrlPiece . fromToParamSchemaOf
  toEncodedUrlPiece = Servant.toEncodedUrlPiece . fromToParamSchemaOf
  toHeader = Servant.toHeader . fromToParamSchemaOf
  toQueryParam = Servant.toQueryParam . fromToParamSchemaOf


instance Servant.FromHttpApiData a => Servant.FromHttpApiData (ToParamSchemaOf _schema a) where
  parseUrlPiece = fmap ToParamSchemaOf . Servant.parseUrlPiece
  parseHeader = fmap ToParamSchemaOf . Servant.parseHeader
  parseQueryParam = fmap ToParamSchemaOf . Servant.parseQueryParam
