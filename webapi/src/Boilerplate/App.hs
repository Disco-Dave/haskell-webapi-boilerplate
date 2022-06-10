module Boilerplate.App (
  AppData (..),
  App (..),
  runApp,
  HttpApp (..),
  liftApp,
  toHandler,
) where

import Boilerplate.Logging (LoggingData)
import qualified Boilerplate.Logging as Logging
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks, local, runReaderT)
import Data.Coerce (coerce)
import Data.Pool (Pool)
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Katip
import qualified Servant
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (catch, throwIO, try)


data AppData = AppData
  { loggingData :: LoggingData
  , postgresConnectionPool :: Pool Postgres.Connection
  }


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


newtype HttpApp a = HttpApp
  { fromHttpApp :: App a
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


liftApp :: App a -> HttpApp a
liftApp =
  coerce


toHandler :: AppData -> HttpApp a -> Servant.Handler a
toHandler appData (HttpApp app) =
  let unwrappedValue = try $ runApp appData app
   in Servant.Handler $ ExceptT unwrappedValue


instance MonadError Servant.ServerError HttpApp where
  throwError = throwIO
  catchError = catch
