module Boilerplate.Database (
  DatabaseUrl (..),
  NumberOfStripes (..),
  UnusedConnectionTimeout (..),
  MaxConnectionsPerStripe (..),
  DatabaseConfig (..),
  withPostgresConnectionPool,
  withPostgresConnection,
) where

import Boilerplate.App (App)
import qualified Boilerplate.App as App
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.String (IsString)
import Data.Time (NominalDiffTime)
import qualified Database.PostgreSQL.Simple as Postgres
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


withPostgresConnectionPool :: DatabaseConfig -> (Pool Postgres.Connection -> IO a) -> IO a
withPostgresConnectionPool DatabaseConfig{..} use =
  let makePool =
        Pool.createPool
          (Postgres.connectPostgreSQL $ coerce url)
          Postgres.close
          (coerce numberOfStripes)
          (coerce unusedConnectionTimeout)
          (coerce maxConnectionsPerStripe)
   in UnliftIO.bracket makePool Pool.destroyAllResources use


withPostgresConnection :: (Postgres.Connection -> IO a) -> App a
withPostgresConnection use = do
  pool <- asks App.postgresConnectionPool
  liftIO $ Pool.withResource pool use
