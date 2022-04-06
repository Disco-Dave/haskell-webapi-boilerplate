module Boilerplate.Api (
  ApiConfig (..),
  makeWaiApplication,
  start,
) where

import qualified Boilerplate.Api.Middleware as Middleware
import qualified Boilerplate.Api.Routes as Routes
import Boilerplate.App (App)
import qualified Boilerplate.App as App
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (ask)
import Data.Proxy (Proxy (Proxy))
import qualified Katip
import qualified Katip.Wai
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant (hoistServer, serve)
import qualified UnliftIO

data ApiConfig = ApiConfig
  { useSwagger :: Bool
  , port :: Warp.Port
  }
  deriving (Show, Eq)

makeWaiApplication :: Bool -> App Wai.Application
makeWaiApplication useSwagger = do
  appData <- ask

  let toApplication api apiServer =
        let app request send =
              let hoistedApp =
                    let hoistedServer = hoistServer api (App.toHandler appData) apiServer
                     in serve api hoistedServer
               in UnliftIO.withRunInIO $ \toIO ->
                    hoistedApp request (toIO . send)
         in Middleware.apply app

  pure . Katip.Wai.runApplication (App.runApp appData) $
    if useSwagger
      then toApplication (Proxy @Routes.ApiWithSwagger) Routes.serverWithSwagger
      else toApplication (Proxy @Routes.Api) Routes.server

start :: ApiConfig -> App ()
start ApiConfig{..} = do
  application <- makeWaiApplication useSwagger

  UnliftIO.withRunInIO $ \toIO ->
    let beforeMainLoop = do
          let uri = "http://localhost:" <> Katip.ls (show port)

          toIO . Katip.logLocM Katip.InfoS $ "Server running at " <> uri

          when useSwagger $
            toIO . Katip.logLocM Katip.InfoS $ "Visit Swagger at " <> uri <> "/docs"

        settings =
          Warp.defaultSettings
            & Warp.setBeforeMainLoop beforeMainLoop
            & Warp.setPort port
            & Warp.setOnException (\_ _ -> pure ()) -- omit logging because middleware should have done this already
     in Warp.runSettings settings application
