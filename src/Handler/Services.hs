{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Services
    ( ServicesAPI
    , servicesServer
    ) where

import qualified Model.Services as M
import qualified Data.ByteString.Lazy.Char8 as L
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
               , DeleteNoContent
               , NoContent(NoContent)
               , (:>)
               , err403
               , err500
               , errBody
               , throwError
               , (:<|>)((:<|>))
               )

type ServicesAPI = "users" :> Capture "username" String
                           :> "services"
                           :> Get '[JSON] [M.Service]
              :<|> "users" :> Header "name" String
                           :> Capture "username" String
                           :> "services"
                           :> ReqBody '[JSON] M.Service
                           :> PostCreated '[JSON] NoContent
              :<|> "users" :> Header "name" String
                           :> Capture "username" String
                           :> "services"
                           :> Capture "service_name" String
                           :> DeleteNoContent '[JSON] NoContent

servicesServer :: Server ServicesAPI
servicesServer = getServices
            :<|> createService
            :<|> deleteService

getServices :: String -> ExceptT ServantErr IO [M.Service]
getServices username =
    (liftIO $ M.getServicesOf username) >>= \case
        Right svcs -> return svcs
        Left err -> throwError $ err500 { errBody = L.pack err }

createService :: Maybe String -> String -> M.Service -> ExceptT ServantErr IO NoContent
createService mhUname pUname svc = do
    let s = svc { M.owner = pUname }
    case mhUname of
        Just hUname | hUname == pUname -> (liftIO $ M.createService s) >>= \case
            Right _ -> return NoContent
            Left err -> throwError $ err500 { errBody = L.pack err }
        _ -> throwError err403

deleteService :: Maybe String -> String -> String -> ExceptT ServantErr IO NoContent
deleteService mhUname pUname svcName = case mhUname of
    Just hUname | hUname == pUname -> (liftIO $ M.deleteService pUname svcName) >>= \case
        Right _ -> return NoContent
        Left err -> throwError $ err500 { errBody = L.pack err }
    _ -> throwError err403
