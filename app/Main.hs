{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Data.String (fromString)
import Handler.Myself (MyselfAPI, myselfServer)
import Handler.Users (UsersAPI, usersServer)
import Handler.Deployments (DeploymentsAPI, deploymentsServer)
import Network.Wai.Handler.Warp (setHost, setPort, runSettings, defaultSettings)
import Servant (Application, Server(..), Proxy(Proxy), serve, (:<|>)((:<|>)))

type TheseusAPI = MyselfAPI
             :<|> UsersAPI
             :<|> DeploymentsAPI

server :: Server TheseusAPI
server = myselfServer
    :<|> usersServer
    :<|> deploymentsServer

app :: Application
app = serve (Proxy :: Proxy TheseusAPI) server

main :: IO ()
main = do
    runSettings (setHost (fromString "127.0.0.1") $ setPort 8080 defaultSettings) app
