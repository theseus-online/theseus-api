{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.String (fromString)
import Handler.Swagger (SwaggerAPI, swaggerServer)
import Handler.Myself (MyselfAPI, myselfServer)
import Handler.Users (UsersAPI, usersServer)
import Handler.Deployments (DeploymentsAPI, deploymentsServer)
import Handler.Services (ServicesAPI, servicesServer)
import Handler.Ingresses (IngressesAPI, ingressesServer)
import Handler.Volumes (VolumesAPI, volumesServer)
import Network.Wai.Handler.Warp (setHost, setPort, runSettings, defaultSettings)
import Servant (Application, Server, Proxy(Proxy), serve, (:<|>)((:<|>)))

type TheseusAPI = SwaggerAPI
             :<|> MyselfAPI
             :<|> UsersAPI
             :<|> DeploymentsAPI
             :<|> ServicesAPI
             :<|> IngressesAPI
             :<|> VolumesAPI

server :: Server TheseusAPI
server = swaggerServer
    :<|> myselfServer
    :<|> usersServer
    :<|> deploymentsServer
    :<|> servicesServer
    :<|> ingressesServer
    :<|> volumesServer

app :: Application
app = serve (Proxy :: Proxy TheseusAPI) server

main :: IO ()
main = do
    runSettings (setHost (fromString "127.0.0.1") $ setPort 8080 defaultSettings) app
