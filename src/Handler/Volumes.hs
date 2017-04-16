{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Volumes
    ( VolumesAPI
    , volumesServer
    ) where

import Model.Settings (volumeRoot)
import System.FilePath ((</>))
import qualified Model.Volumes as M
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Servant ( Get
               , Raw
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
               , serveDirectory
               , (:<|>)((:<|>))
               )

type VolumesAPI = "users" :> Capture "username" String
                          :> "volumes"
                          :> Get '[JSON] [M.Volume]
             :<|> "users" :> Capture "username"  String
                          :> "volumes"
                          :> Capture "volume" String
                          :> Get '[JSON] M.Content
             :<|> "users" :> Capture "username"  String
                          :> "volumes"
                          :> Capture "volume" String
                          :> "files"
                          :> Raw
             :<|> "users" :> Header "name" String
                          :> Capture "username" String
                          :> "volumes"
                          :> ReqBody '[JSON] M.Volume
                          :> PostCreated '[JSON] NoContent
             :<|> "users" :> Header "name" String
                          :> Capture "username" String
                          :> "volumes"
                          :> Capture "volume_name" String
                          :> DeleteNoContent '[JSON] NoContent

volumesServer :: Server VolumesAPI
volumesServer = getVolumes
           :<|> getVolumeContent
           :<|> getFileContent
           :<|> createVolume
           :<|> deleteVolume

getVolumes :: String -> ExceptT ServantErr IO [M.Volume]
getVolumes username = do
    r <- liftIO $ M.getVolumesOf username
    case r of
        Right vs -> return vs
        Left err -> throwError $ err500 { errBody = L.pack err }

getVolumeContent :: String -> String -> ExceptT ServantErr IO M.Content
getVolumeContent username vName = do
    r <- liftIO $ M.getVolumeContent username vName
    case r of
        Right c -> return c
        Left err -> throwError $ err500 { errBody = L.pack err }

getFileContent :: String -> String -> Server Raw
getFileContent username vName = serveDirectory $ volumeRoot </> username </> vName

createVolume :: Maybe String -> String -> M.Volume -> ExceptT ServantErr IO NoContent
createVolume mhUname pUname volume = do
    let v = volume { M.owner = pUname }
    case mhUname of
        Just hUname | hUname == pUname -> (liftIO $ M.createVolume v) >>= \case
            Right _ -> return NoContent
            Left err -> throwError $ err500 { errBody = L.pack err }
        _ -> throwError err403

deleteVolume :: Maybe String -> String -> String -> ExceptT ServantErr IO NoContent
deleteVolume mhUname pUname vName = case mhUname of
    Just hUname | hUname == pUname -> (liftIO $ M.deleteVolume pUname vName) >>= \case
        Right _ -> return NoContent
        Left err -> throwError $ err500 { errBody = L.pack err }
    _ -> throwError err403
