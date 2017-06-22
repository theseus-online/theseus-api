{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Swagger
    ( SwaggerAPI
    , swaggerServer
    ) where

import Data.FileEmbed (embedFile)
import Servant (Server, Raw, (:>))
import Network.Wai (responseLBS)
import qualified Data.ByteString.Lazy as LB
import Network.HTTP.Types.Status (status200)

type SwaggerAPI = "swagger.json" :> Raw

swaggerServer :: Server Raw
swaggerServer _ respond = respond $ responseLBS
                                        status200
                                        [("Content-Type", "application/json")]
                                        (LB.fromStrict $(embedFile "swagger.json"))
