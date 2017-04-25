{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Volumes
    ( VolumesAPI
    , volumesServer
    ) where

import Data.List (foldl')
import Model.Settings (volumeRoot)
import System.FilePath ((</>))
import System.Directory (removeFile, createDirectoryIfMissing, removeDirectoryRecursive)
import Network.Wai (Request(requestMethod, pathInfo, requestBody), responseLBS)
import qualified Model.Volumes as M
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Network.HTTP.Types.Status (status204, status403, status405)
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
             :<|> "users" :> Header "x-theseus-username" String
                          :> Capture "username"  String
                          :> "volumes"
                          :> Capture "volume" String
                          :> "files"
                          :> Raw
             :<|> "users" :> Header "x-theseus-username" String
                          :> Capture "username"  String
                          :> "volumes"
                          :> Capture "volume" String
                          :> "folders"
                          :> Raw
             :<|> "users" :> Header "x-theseus-username" String
                          :> Capture "username" String
                          :> "volumes"
                          :> ReqBody '[JSON] M.Volume
                          :> PostCreated '[JSON] NoContent
             :<|> "users" :> Header "x-theseus-username" String
                          :> Capture "username" String
                          :> "volumes"
                          :> Capture "volume_name" String
                          :> DeleteNoContent '[JSON] NoContent

volumesServer :: Server VolumesAPI
volumesServer = getVolumes
           :<|> getVolumeContent
           :<|> serveFile
           :<|> serveFolder
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

serveFile :: Maybe String -> String -> String -> Server Raw
serveFile mhUname pUname vName =
    case mhUname of
        Just hUname | hUname == pUname -> fileHandler
        _ -> e403Handler

    where
        fileHandler req respond = case requestMethod req of
            "GET" -> (serveDirectory rootPath) req respond
            "HEAD" -> (serveDirectory rootPath) req respond
            "PUT" -> uploadHandler req respond
            "DELETE" -> removehandler req respond
            otherwise -> respond $ responseLBS status405 [] "Only GET/HEAD/PUT/DELETE allowed."

        uploadHandler req respond = do
            let ps = pathInfo req
            let fpath = foldl' (\p i -> p </> (T.unpack i)) rootPath ps
            liftIO (requestBody req >>= B.writeFile fpath)
            respond $ responseLBS status204 [] ""

        removehandler req respond = do
            let ps = pathInfo req
            let fpath = foldl' (\p i -> p </> (T.unpack i)) rootPath ps
            liftIO $ removeFile fpath
            respond $ responseLBS status204 [] ""

        e403Handler _ respond = respond $ responseLBS status403 [] "Forbiden"

        rootPath = volumeRoot </> pUname </> vName

serveFolder :: Maybe String -> String -> String -> Server Raw
serveFolder mhUname pUname vName =
    case mhUname of
        Just hUname | hUname == pUname -> folderHandler
        _ -> e403Handler

    where
        folderHandler req respond = case requestMethod req of
            "PUT" -> uploadHandler req respond
            "DELETE" -> removehandler req respond
            otherwise -> respond $ responseLBS status405 [] "Only PUT/DELETE allowed."

        uploadHandler req respond = do
            let ps = pathInfo req
            let fpath = foldl' (\p i -> p </> (T.unpack i)) rootPath ps
            liftIO $ createDirectoryIfMissing True fpath
            respond $ responseLBS status204 [] ""

        removehandler req respond = do
            let ps = pathInfo req
            let fpath = foldl' (\p i -> p </> (T.unpack i)) rootPath ps
            liftIO $ removeDirectoryRecursive fpath
            respond $ responseLBS status204 [] ""

        e403Handler _ respond = respond $ responseLBS status403 [] "Forbiden"

        rootPath = volumeRoot </> pUname </> vName

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
