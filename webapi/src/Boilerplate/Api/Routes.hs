module Boilerplate.Api.Routes (
  Api,
  server,
  ApiWithSwagger,
  serverWithSwagger,
) where

import Boilerplate.App (ApiApp)
import qualified Boilerplate.HealthChecks.Api as HealthChecks
import Control.Lens ((.~), (?~))
import Data.Function ((&))
import Data.OpenApi (OpenApi)
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Operation (applyTagsFor)
import Servant (HasServer (ServerT), Proxy (Proxy), type (:<|>) (..), type (:>))
import Servant.OpenApi (subOperations, toOpenApi)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServerT)

type HealthChecks = "health-checks" :> HealthChecks.Api

type Api =
  HealthChecks

server :: ServerT Api ApiApp
server =
  HealthChecks.server

type ApiWithSwagger =
  SwaggerSchemaUI "docs" "openapi.json" :<|> Api

openApi :: OpenApi
openApi =
  let tagRoutes proxy name description =
        applyTagsFor
          (subOperations @_ @Api proxy Proxy)
          [name & OpenApi.description ?~ description]
   in toOpenApi @Api Proxy
        & OpenApi.info . OpenApi.title .~ "Boilerplate"
        & OpenApi.info . OpenApi.description ?~ "Template for common boilerplate I find I need when starting a web api"
        & OpenApi.info . OpenApi.version .~ "0.1.0"
        & tagRoutes (Proxy @HealthChecks) "Health Checks" "End points for checking the health of the API"

serverWithSwagger :: ServerT ApiWithSwagger ApiApp
serverWithSwagger =
  swaggerSchemaUIServerT openApi :<|> server
