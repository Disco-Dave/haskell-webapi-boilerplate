{-# LANGUAGE DerivingVia #-}

module Boilerplate.Database (
  DatabaseUrl (..),
  NumberOfStripes (..),
  UnusedConnectionTimeout (..),
  MaxConnectionsPerStripe (..),
  DatabaseConfig (..),
  ConnectionPool,
  HasConnectionPool (..),
  withConnectionPool,
  withConnection,
) where

import Control.Monad.Reader (MonadReader, asks)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.String (IsString)
import Data.Time (NominalDiffTime)
import qualified Database.PostgreSQL.Simple as Postgres
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO


newtype DatabaseUrl = DatabaseUrl
  { fromDatabaseUrl :: ByteString
  }
  deriving (Show, Eq, IsString) via ByteString


newtype NumberOfStripes = NumberOfStripes
  { fromNumberOfStripes :: Int
  }
  deriving (Show, Eq, Num, Read) via Int


newtype UnusedConnectionTimeout = UnusedConnectionTimeout
  { fromUnusedConnectionTimeout :: NominalDiffTime
  }
  deriving (Show, Eq, Num, Read) via NominalDiffTime


newtype MaxConnectionsPerStripe = MaxConnectionsPerStripe
  { fromMaxConnectionsPerStripe :: Int
  }
  deriving (Show, Eq, Num, Read) via Int


data DatabaseConfig = DatabaseConfig
  { url :: DatabaseUrl
  , numberOfStripes :: NumberOfStripes
  , unusedConnectionTimeout :: UnusedConnectionTimeout
  , maxConnectionsPerStripe :: MaxConnectionsPerStripe
  }


newtype ConnectionPool = ConnectionPool (Pool Postgres.Connection)


withConnectionPool :: DatabaseConfig -> (ConnectionPool -> IO a) -> IO a
withConnectionPool DatabaseConfig{..} use =
  let makePool =
        Pool.createPool
          (Postgres.connectPostgreSQL $ coerce url)
          Postgres.close
          (coerce numberOfStripes)
          (coerce unusedConnectionTimeout)
          (coerce maxConnectionsPerStripe)
   in UnliftIO.bracket makePool Pool.destroyAllResources (use . ConnectionPool)


class HasConnectionPool r where
  getConnectionPool :: r -> ConnectionPool


withConnection :: (HasConnectionPool r, MonadReader r m, MonadUnliftIO m) => (Postgres.Connection -> m a) -> m a
withConnection use = do
  ConnectionPool pool <- asks getConnectionPool
  UnliftIO.withRunInIO $ \toIO ->
    Pool.withResource pool (toIO . use)
