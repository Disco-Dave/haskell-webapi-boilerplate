module Boilerplate.Database (
  DatabaseUrl (..),
  DatabaseConfig (..),
  PoolCacheTtl (..),
  PoolMaxResources (..),
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
import qualified Database.PostgreSQL.Simple as Postgres
import qualified UnliftIO


newtype DatabaseUrl = DatabaseUrl
  { fromDatabaseUrl :: ByteString
  }
  deriving (Show, Eq, IsString) via ByteString


newtype PoolCacheTtl = PoolCacheTtl
  { fromPoolCacheTtl :: Double
  }
  deriving (Show, Eq, Num, Read) via Double


newtype PoolMaxResources = PoolMaxResources
  { fromPoolMaxResources :: Int
  }
  deriving (Show, Eq, Num, Read) via Int


data DatabaseConfig = DatabaseConfig
  { url :: DatabaseUrl
  , poolCacheTtl :: PoolCacheTtl
  , poolMaxResources :: PoolMaxResources
  }


withPostgresConnectionPool :: DatabaseConfig -> (Pool Postgres.Connection -> IO a) -> IO a
withPostgresConnectionPool DatabaseConfig{..} use =
  let makePool =
        Pool.newPool
          Pool.PoolConfig
            { createResource = Postgres.connectPostgreSQL $ coerce url
            , freeResource = Postgres.close
            , poolCacheTTL = coerce poolCacheTtl
            , poolMaxResources = coerce poolMaxResources
            }
   in UnliftIO.bracket makePool Pool.destroyAllResources use


withPostgresConnection :: (Postgres.Connection -> IO a) -> App a
withPostgresConnection use = do
  pool <- asks App.postgresConnectionPool
  liftIO $ Pool.withResource pool use
