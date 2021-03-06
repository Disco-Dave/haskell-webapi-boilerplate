module Boilerplate.HealthChecks.HttpSpec (spec) where

import qualified Network.HTTP.Client as Http
import Test.Hspec (Spec, aroundAll, it, runIO, shouldBe)
import TestApp (TestApp (..), withTestApp)


spec :: Spec
spec = do
  manager <- runIO $ Http.newManager Http.defaultManagerSettings

  let responds204 rawRequest = do
        request <- Http.parseRequest rawRequest
        response <- Http.httpNoBody request manager

        let status = fromEnum $ Http.responseStatus response
         in status `shouldBe` 204

  aroundAll withTestApp $ do
    it "GET /health-checks responds with 204" $ \TestApp{port} ->
      responds204 $ "http://localhost:" <> show port <> "/health-checks"

    it "GET /health-checks/databases responds with 204" $ \TestApp{port} ->
      responds204 $ "http://localhost:" <> show port <> "/health-checks/databases"
