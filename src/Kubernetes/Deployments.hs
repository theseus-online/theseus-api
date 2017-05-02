{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Kubernetes.Deployments
    ( getDeployments
    , getDeploymentsOf
    , createDeployment
    , deleteDeployment
    , Deployment(Deployment)
    , Container(Container)
    , Volume(Volume)
    ) where

import System.FilePath ((</>))
import GHC.Exts (fromList)
import Data.List (nub)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Network.Wreq (get, post, delete, deleteWith, param, defaults, responseBody, responseStatus, statusCode)
import Control.Lens ((&), (.~), (^.), (^?), (^..))
import Kubernetes.Settings (deployments, deploymentOf, deploymentsOf, replicasetsOf, podsOf, volumeRoot)
import Data.Aeson ((.:), (.!=), (.:?), (.=), encode, decode, object, FromJSON(..), Value(..))

data DeploymentResult = DeploymentResult [Deployment] deriving (Show)

instance FromJSON DeploymentResult where
    parseJSON = \case
        Object o -> (o .: "items") >>= fmap DeploymentResult . parseJSON
        x -> fail $ "unexpected json: " ++ show x

data Deployment = Deployment
                { name :: String
                , namespace :: String
                , containers :: [Container]
                } deriving (Show)

instance FromJSON Deployment where
    parseJSON = \case
        Object o -> Deployment
                <$> ((o .: "metadata") >>= (.: "name"))
                <*> ((o .: "metadata") >>= (.: "namespace"))
                <*> ((o .: "spec") >>= (.: "template") >>= (.: "spec") >>= (.: "containers") >>= parseJSON)
        x -> fail $ "unexpected json: " ++ show x

data Container = Container
               { name :: String
               , image ::String
               , volumes :: [Volume]
               } deriving (Show)

instance FromJSON Container where
    parseJSON = \case
        Object o -> Container
                <$> (o .: "name")
                <*> (o .: "image")
                <*> (((o .:? "volumeMounts")) .!= (Array $ fromList []) >>= parseJSON)
        x -> fail $ "unexpected json: " ++ show x

data Volume = Volume
            { name :: String
            , mountPath :: String
            } deriving (Show)

instance FromJSON Volume where
    parseJSON = \case
        Object o -> Volume
                <$> (o .: "name")
                <*> (o .: "mountPath")
        x -> fail $ "unexpected json: " ++ show x

getDeployments :: IO (Either String [Deployment])
getDeployments = do
    r <- get deployments
    case decode (r ^. responseBody) of
        Just (DeploymentResult deps) -> return $ Right deps
        Nothing -> return $ Left $ "request kubernetes failed" ++ show (r ^. responseBody)

getDeploymentsOf :: String -> IO (Either String [Deployment])
getDeploymentsOf namespace = do
    r <- get $ deploymentsOf namespace
    case decode (r ^. responseBody) of
        Just (DeploymentResult deps) -> return $ Right deps
        Nothing -> return $ Left $ "request kubernetes failed" ++ show (r ^. responseBody)

createDeployment :: Deployment -> IO (Either String ())
createDeployment dep = do
    let d = object [ "metadata" .= deploymentMeta dep
                   , "spec" .= deploymentSpec dep
                   ]
    post ((deploymentsOf . namespace) dep) d
    return $ Right ()

    where
        deploymentMeta (Deployment nm ns _) = object [ "name" .= nm
                                                     , "namespace" .= ns
                                                     ]

        deploymentSpec (Deployment nm ns cs) = object [
            "template" .= object [ "metadata" .= object ["labels" .= object ["app" .= nm]]
                                 , "spec" .= object [ "containers" .= deploymentContainers cs
                                                    , "volumes" .= deploymentVolumes ns (foldContainerVolumes cs)
                                                    ]]]

        foldContainerVolumes cs = concat $ map (\(Container _ _ vs) -> map (\(Volume n _) -> n) vs) cs

        deploymentVolumes ns vs = map (\v -> object [ "name" .= v
                                                    , if take 9 v == "empty-dir"
                                                        then ("emptyDir" .= object [])
                                                        else ("hostPath" .= object ["path" .= (volumeRoot </> ns </> v)])
                                                    ]) (nub vs)

        deploymentContainers cs = map (\(Container n i vs) -> object [ "name" .= n
                                                                     , "image" .= i
                                                                     , "volumeMounts" .= containerVolumes vs]) cs

        containerVolumes vs = map(\(Volume n m) -> object ["name" .= n, "mountPath" .= m]) vs

deleteDeployment :: String -> String -> IO (Either String ())
deleteDeployment namespace name = do
    let opts = defaults & param "labelSelector" .~ [T.pack $ "app=" ++ name]
    delete $ deploymentOf namespace name
    deleteWith opts $ replicasetsOf namespace
    deleteWith opts $ podsOf namespace
    return $ Right ()
