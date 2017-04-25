{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Ingresses
    ( IngressesAPI
    , ingressesServer
    ) where

import qualified Model.Ingresses as M
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

type IngressesAPI = "users" :> Capture "username" String
                            :> "ingresses"
                            :> Get '[JSON] [M.Ingress]
               :<|> "users" :> Header "x-theseus-username" String
                            :> Capture "username" String
                            :> "ingresses"
                            :> ReqBody '[JSON] M.Ingress
                            :> PostCreated '[JSON] NoContent
               :<|> "users" :> Header "x-theseus-username" String
                            :> Capture "username" String
                            :> "ingresses"
                            :> Capture "ingress_name" String
                            :> DeleteNoContent '[JSON] NoContent

ingressesServer :: Server IngressesAPI
ingressesServer = getIngresses
             :<|> createIngress
             :<|> deleteIngress

getIngresses :: String -> ExceptT ServantErr IO [M.Ingress]
getIngresses username =
    (liftIO $ M.getIngressesOf username) >>= \case
        Right ings -> return ings
        Left err -> throwError $ err500 { errBody = L.pack err }

createIngress :: Maybe String -> String -> M.Ingress -> ExceptT ServantErr IO NoContent
createIngress mhUname pUname ing = do
    let i = ing { M.owner = pUname }
    case mhUname of
        Just hUname | hUname == pUname -> (liftIO $ M.createIngress i) >>= \case
            Right _ -> return NoContent
            Left err -> throwError $ err500 { errBody = L.pack err }
        _ -> throwError err403

deleteIngress :: Maybe String -> String -> String -> ExceptT ServantErr IO NoContent
deleteIngress mhUname pUname ingName = case mhUname of
    Just hUname | hUname == pUname -> (liftIO $ M.deleteIngress pUname ingName) >>= \case
        Right _ -> return NoContent
        Left err -> throwError $ err500 { errBody = L.pack err }
    _ -> throwError err403
