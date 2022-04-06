module Boilerplate.Api.Middleware (
  apply,
) where

import Boilerplate.App (App)
import Control.Monad.IO.Class (liftIO)
import qualified Katip
import Katip.Wai (ApplicationT, MiddlewareT)
import qualified Katip.Wai
import Network.Wai (Application, Middleware)
import qualified UnliftIO

unliftApplication :: ApplicationT App -> App Application
unliftApplication application = do
  UnliftIO.withRunInIO $ \toIO ->
    pure $ \req send -> toIO $ application req (liftIO . send)

liftMiddleware :: Middleware -> MiddlewareT App
liftMiddleware middleware application request send = do
  unliftedApplication <- unliftApplication application
  UnliftIO.withRunInIO $ \toIO ->
    middleware unliftedApplication request (toIO . send)

logExceptions :: MiddlewareT App
logExceptions application request send =
  Katip.logExceptionM (application request send) Katip.ErrorS

logRequestsAndResponses :: MiddlewareT App
logRequestsAndResponses =
  Katip.Wai.middleware Katip.InfoS

middlewares :: [MiddlewareT App]
middlewares =
  [ logRequestsAndResponses
  , logExceptions
  ]

apply :: MiddlewareT App
apply original =
  foldr ($) original middlewares
