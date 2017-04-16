{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Volumes
    ( getVolumes
    , getVolumesOf
    , getVolumeContent
    , createVolume
    , deleteVolume
    , Content
    , Volume(..)
    ) where

import GHC.Generics (Generic)
import Control.Monad (forM)
import Model.Settings (volumeRoot)
import Data.Aeson (ToJSON(toJSON), FromJSON, object, (.=))
import System.FilePath ((</>))
import System.Directory
    ( doesDirectoryExist
    , getDirectoryContents
    , createDirectory
    , removeDirectoryRecursive
    )
import System.FilePath.Posix(takeFileName)

data Content = File String | Directory String [Content] deriving (Show)

instance ToJSON Content where
    toJSON (File name) =
        object [ "name" .= name, "type" .= ("file" :: String) ]

    toJSON (Directory name cs) =
        object [ "name" .= name, "type" .= ("folder" :: String), "children" .= toJSON cs]

data Volume = Volume
            { name :: String
            , owner :: String
            } deriving (Show, Generic)

instance ToJSON Volume
instance FromJSON Volume

getRecursiveContents :: FilePath -> IO Content
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return $ File (takeFileName path)
    return $ Directory (takeFileName topdir) paths

getVolumes :: IO (Either String [Volume])
getVolumes = getRecursiveContents volumeRoot >>= \case
    Directory _ ds -> do
        let vss = (`map` ds) $ \(Directory dp vs) ->
                  (`map` vs) $ \(Directory vp _)  ->
                      Volume vp dp
        return (Right $ concat vss)

getVolumesOf :: String -> IO (Either String [Volume])
getVolumesOf username = getRecursiveContents (volumeRoot </> username) >>= \case
    Directory _ vs -> do
        return $ Right $ (`map` vs) $ \(Directory vp _) -> Volume (takeFileName vp) username

createVolume :: Volume -> IO (Either String ())
createVolume v = do
    createDirectory $ volumeRoot </> (owner v) </> (name v)
    return $ Right ()

deleteVolume :: String -> String -> IO (Either String ())
deleteVolume username volume = do
    removeDirectoryRecursive $ volumeRoot </> username </> volume
    return $ Right ()

getVolumeContent :: String -> String -> IO (Either String Content)
getVolumeContent username volume =
    fmap (\c -> Right c) $ getRecursiveContents (volumeRoot </> username </> volume)
