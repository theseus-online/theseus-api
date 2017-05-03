{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Deployments
    ( DeploymentsAPI
    , deploymentsServer
    ) where

import qualified Model.Deployments as M
import qualified Model.Logs as M
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Network.Wai (responseLBS)
import Network.HTTP.Types.Status (status200, status404)
import Servant ( Get
               , Post
               , JSON
               , Raw
               , Server
               , Header
               , Proxy(..)
               , Capture
               , ReqBody
               , ServantErr
               , PostCreated
               , DeleteNoContent
               , NoContent(NoContent)
               , (:>)
               , err403
               , err500
               , errBody
               , throwError
               , (:<|>)((:<|>))
               )

type DeploymentsAPI = "users" :> Capture "username" String
                              :> "deployments"
                              :> Get '[JSON] [M.Deployment]
                 :<|> "users" :> Header "x-theseus-username" String
                              :> Capture "username" String
                              :> "deployments"
                              :> ReqBody '[JSON] M.Deployment
                              :> PostCreated '[JSON] NoContent
                 :<|> "users" :> Header "x-theseus-username" String
                              :> Capture "username" String
                              :> "deployments"
                              :> Capture "deployment_name" String
                              :> DeleteNoContent '[JSON] NoContent
                 :<|> "users" :> Capture "username" String
                              :> "pods"
                              :> Capture "pod" String
                              :> "containers"
                              :> Capture "container" String
                              :> "logs"
                              :> Raw

deploymentsServer :: Server DeploymentsAPI
deploymentsServer = getDeployments
               :<|> createDeployment
               :<|> deleteDeployment
               :<|> getLogs

getLogs :: String -> String -> String -> Server Raw
getLogs username pod container = getLogsOfPod
    where
        getLogsOfPod _ respond = M.getLogsOf username pod container >>= \case
            Just logs -> respond $ responseLBS status200 [] logs
            Nothing -> respond $ responseLBS status404 [] "no such container"

getDeployments :: String -> ExceptT ServantErr IO [M.Deployment]
getDeployments username = do
    r <- liftIO $ M.getDeploymentsOf username
    case r of
        Right deps -> return deps
        Left err -> throwError $ err500 { errBody = L.pack err }

createDeployment :: Maybe String -> String -> M.Deployment -> ExceptT ServantErr IO NoContent
createDeployment mhUname pUname dep = do
    let d = dep { M.owner = pUname }
    case mhUname of
        Just hUname | hUname == pUname -> (liftIO $ M.createDeployment d) >>= \case
            Right _ -> return NoContent
            Left err -> throwError $ err500 { errBody = L.pack err }
        _ -> throwError err403

deleteDeployment :: Maybe String -> String -> String -> ExceptT ServantErr IO NoContent
deleteDeployment mhUname pUname depName = case mhUname of
    Just hUname | hUname == pUname -> (liftIO $ M.deleteDeployment pUname depName) >>= \case
        Right _ -> return NoContent
        Left err -> throwError $ err500 { errBody = L.pack err }
    _ -> throwError err403
