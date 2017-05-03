{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Kubernetes.Pods
    ( getPods
    , getPodsOf
    , getLogsOf
    , Pod(..)
    , ContainerInstance(..)
    , ContainerStatus(..)
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Internal as B
import Kubernetes.Settings (pods, podsOf, logsOf)
import qualified Data.HashMap.Strict as HM
import GHC.Exts (fromList)
import Control.Lens ((&), (.~), (^.), (^?), (^..))
import Data.Aeson ((.:), (.:?), (.!=), (.=), encode, decode, object, FromJSON(..), Value(..))
import Network.Wreq (get, getWith, param, defaults, responseBody, responseStatus, statusCode)

data PodResult = PodResult [Pod] deriving (Show)

instance FromJSON PodResult where
    parseJSON = \case
        Object o -> (o .: "items") >>= fmap PodResult . parseJSON
        x -> fail $ "unexpected json: " ++ show x

data Pod = Pod
         { name :: String
         , app :: String                -- belong to which deployment
         , namespace :: String
         , podIP :: String
         , containers :: [ContainerInstance]
         } deriving (Show)

instance FromJSON Pod where
    parseJSON = \case
        Object o -> Pod
                <$> ((o .: "metadata") >>= (.: "name"))
                <*> ((o .: "metadata") >>= (.: "labels") >>= (.: "app"))
                <*> ((o .: "metadata") >>= (.: "namespace"))
                <*> (((o .: "status") >>= (.:? "podIP")) .!= "")
                <*> ((o .: "status") >>= (.: "containerStatuses") >>= parseJSON)

data ContainerInstance = ContainerInstance
                       { name :: String
                       , status :: ContainerStatus
                       } deriving (Show)

instance FromJSON ContainerInstance where
    parseJSON = \case
        Object o -> ContainerInstance
                <$> (o .: "name")
                <*> ((o .: "state") >>= parseJSON)

data ContainerStatus = Running { startedAt :: String }
                     | Waiting
                       { reason :: String
                       , message :: String
                       }
                     | Terminated
                       { startedAt :: String
                       , finishedAt :: String
                       , exitCode :: Int
                       , reason :: String
                       , message :: String
                       }
                     | Unknown
                     deriving (Show)

instance FromJSON ContainerStatus where
    parseJSON = \case
        Object o -> maybe (maybe (maybe (return Unknown)
                                     parseTerminated (isTerminated o))
                              parseWaiting (isWaiting o))
                          parseRunning (isRunning o)
        where
            isRunning = HM.lookup ("running" :: T.Text)
            isWaiting = HM.lookup ("waiting" :: T.Text)
            isTerminated = HM.lookup ("terminated" :: T.Text)

            parseRunning (Object o) = Running <$> (o .: "startedAt")
            parseWaiting (Object o) = Waiting
                                  <$> (o .:? "reason" .!= "")
                                  <*> (o .:? "message" .!= "")
            parseTerminated (Object o) = Terminated
                                     <$> (o .: "startedAt")
                                     <*> (o .: "finishedAt")
                                     <*> (o .: "exitCode")
                                     <*> (o .:? "reason" .!= "")
                                     <*> (o .:? "message" .!= "")
--}
getPods :: IO (Either String [Pod])
getPods = do
    r <- get pods
    case decode (r ^. responseBody) of
        Just (PodResult pods) -> return $ Right pods
        Nothing -> return $ Left $ "request kubernetes failed" ++ show (r ^. responseBody)

getPodsOf :: String -> IO (Either String [Pod])
getPodsOf namespace = do
    r <- get $ podsOf namespace
    case decode (r ^. responseBody) of
        Just (PodResult pods) -> return $ Right pods
        Nothing -> return $ Left $ "request kubernetes failed" ++ show (r ^. responseBody)

getLogsOf :: String -> String -> String -> IO (Maybe B.ByteString)
getLogsOf namespace pod container = do
    let opts = defaults & param "container" .~ [T.pack container]
    r <- getWith opts $ logsOf namespace pod
    return $ Just $ r ^. responseBody
