{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Deployments
    ( DeploymentsAPI
    , deploymentsServer
    ) where

import Data.ByteString.Lazy.Char8 as L
import qualified Model.Deployments as M

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Servant ( Get
               , Post
               , JSON
               , Server
               , Header
               , Proxy(..)
               , Capture
               , ReqBody
               , ServantErr
               , PostCreated
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
                 :<|> "users" :> Header "name" String 
                              :> Capture "username" String 
                              :> "deployments"
                              :> ReqBody '[JSON] M.Deployment
                              :> PostCreated '[JSON] NoContent

deploymentsServer :: Server DeploymentsAPI
deploymentsServer = getDeployments
               :<|> createDeployment


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
        Just mhUname | mhUname == pUname -> (liftIO $ M.createDeployment d) >>= \case
            Right _ -> return NoContent
            Left err -> throwError $ err500 { errBody = L.pack err }
        _ -> throwError err403
    