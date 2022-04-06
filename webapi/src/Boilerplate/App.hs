module Boilerplate.App (
  AppData (..),
  App (..),
  runApp,
  ApiApp (..),
  liftApp,
  toHandler,
) where

import Boilerplate.Database (ConnectionPool, HasConnectionPool (..))
import Boilerplate.Logging (LoggingData)
import qualified Boilerplate.Logging as Logging
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, local, runReaderT)
import Data.Coerce (coerce)
import qualified Katip
import qualified Servant
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO

data AppData = AppData
  { loggingData :: LoggingData
  , connectionPool :: ConnectionPool
  }

instance HasConnectionPool AppData where
  getConnectionPool = connectionPool

newtype App a = App
  { fromApp :: ReaderT AppData IO a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader AppData
    , MonadUnliftIO
    , MonadThrow
    , MonadCatch
    )

runApp :: AppData -> App a -> IO a
runApp appData (App app) =
  runReaderT app appData

instance Katip.Katip App where
  getLogEnv =
    asks (Logging.logEnv . loggingData)

  localLogEnv f (App m) =
    let update appData =
          let oldLogEnv = Logging.logEnv $ loggingData appData
              newLoggingData = (loggingData appData){Logging.logEnv = f oldLogEnv}
           in appData{loggingData = newLoggingData}
     in App (local update m)

instance Katip.KatipContext App where
  getKatipContext =
    asks (Logging.context . loggingData)

  localKatipContext f (App m) =
    let update appData =
          let oldContext = Logging.context $ loggingData appData
              newLoggingData = (loggingData appData){Logging.context = f oldContext}
           in appData{loggingData = newLoggingData}
     in App (local update m)

  getKatipNamespace =
    asks (Logging.namespace . loggingData)

  localKatipNamespace f (App m) =
    let update appData =
          let oldNamespace = Logging.namespace $ loggingData appData
              newLoggingData = (loggingData appData){Logging.namespace = f oldNamespace}
           in appData{loggingData = newLoggingData}
     in App (local update m)

newtype ApiApp a = ApiApp
  { fromApiApp :: App a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader AppData
    , MonadUnliftIO
    , MonadThrow
    , MonadCatch
    , Katip.Katip
    , Katip.KatipContext
    )

liftApp :: App a -> ApiApp a
liftApp =
  coerce

toHandler :: AppData -> ApiApp a -> Servant.Handler a
toHandler appData (ApiApp app) =
  let unwrappedValue = UnliftIO.try $ runApp appData app
   in Servant.Handler $ ExceptT unwrappedValue

instance MonadError Servant.ServerError ApiApp where
  throwError = UnliftIO.throwIO
  catchError = UnliftIO.catch
