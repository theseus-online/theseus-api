module Main (main) where

import Data.String (fromString)
import Servant (Application, serve)
import Handler.Myself (myselfAPI, myselfServer)
import Network.Wai.Handler.Warp (setHost, setPort, runSettings, defaultSettings)

app :: Application
app = serve myselfAPI myselfServer

main :: IO ()
main = do
    runSettings (setHost (fromString "127.0.0.1") $ setPort 8080 defaultSettings) app
