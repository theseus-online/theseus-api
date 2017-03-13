module Main (main) where

import Servant (Application, serve)
import Network.Wai.Handler.Warp (run)
import Handler.Myself (myselfAPI, myselfServer)

app :: Application
app = serve myselfAPI myselfServer

main :: IO ()
main = do
    run 8080 app