{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Volumes
    ( VolumesAPI
    , volumesServer
    ) where

import qualified Model.Volumes as M
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

type VolumesAPI = "users" :> Capture "username" String
                          :> "volumes"
                          :> Get '[JSON] [M.Volume]
             :<|> "users" :> Capture "username"  String
                          :> "volumes"
                          :> Capture "volume" String
                          :> Get '[JSON] M.Content

volumesServer :: Server VolumesAPI
volumesServer = getVolumes
           :<|> getVolumeContent

getVolumes :: String -> ExceptT ServantErr IO [M.Volume]
getVolumes username = do
    r <- liftIO $ M.getVolumesOf username
    case r of
        Right vs -> return vs
        Left err -> throwError $ err500 { errBody = L.pack err }

getVolumeContent :: String -> String -> ExceptT ServantErr IO M.Content
getVolumeContent username volume = do
    r <- liftIO $ M.getVolumeContent username volume
    case r of
        Right c -> return c
        Left err -> throwError $ err500 { errBody = L.pack err }
