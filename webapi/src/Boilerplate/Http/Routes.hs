module Boilerplate.Http.Routes (
  Api,
  server,
  ApiWithSwagger,
  serverWithSwagger,
) where

import Boilerplate.App (HttpApp)
import qualified Boilerplate.HealthChecks.Http as HealthChecks
import qualified Boilerplate.Todos.Http as Todos
import Control.Lens ((.~), (?~))
import Data.Function ((&))
import Data.OpenApi (OpenApi)
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Operation (applyTagsFor)
import qualified Data.Text as Text
import Data.Version (showVersion)
import Paths_boilerplate (version)
import Servant (HasServer (ServerT), Proxy (Proxy), type (:<|>) (..), type (:>))
import Servant.OpenApi (subOperations, toOpenApi)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServerT)


type HealthChecks = "health-checks" :> HealthChecks.Api
type Todos = "todos" :> Todos.Api


type Api =
  HealthChecks
    :<|> Todos


server :: ServerT Api HttpApp
server =
  HealthChecks.server
    :<|> Todos.server


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
        & OpenApi.info . OpenApi.version .~ Text.pack (showVersion version)
        & tagRoutes (Proxy @HealthChecks) "Health Checks" "End points for checking the health of the API"
        & tagRoutes (Proxy @Todos) "Todos" "End points for managing todo items"


serverWithSwagger :: ServerT ApiWithSwagger HttpApp
serverWithSwagger =
  swaggerSchemaUIServerT openApi :<|> server
