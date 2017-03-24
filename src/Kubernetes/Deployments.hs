{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Kubernetes.Deployments 
    ( getDeployments
    , getDeploymentsOf
    , Deployment(Deployment)
    , Container(Container)
    ) where

import Data.Text (Text)
import GHC.Exts (fromList)
import Network.Wreq (get, responseBody)
import Control.Lens ((^.), (^?), (^..))
import Kubernetes.Settings (deployments, deploymentsOf)
import Data.Aeson ((.:), (.!=), (.:?), decode, FromJSON(..), Value(..))

data DeploymentResult = DeploymentResult [Deployment] deriving (Show)

instance FromJSON DeploymentResult where
    parseJSON = \case
        Object o -> (o .: "items") >>= fmap DeploymentResult . parseJSON
        x -> fail $ "unexpected json: " ++ show x

data Deployment = Deployment
                { name :: Text
                , namespace :: Text
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
               { name :: Text
               , image ::Text
               , ports :: [ContainerPort]       -- Seems no use?
               } deriving (Show)

instance FromJSON Container where
    parseJSON = \case
        Object o -> Container
                 <$> (o .: "name")
                 <*> (o .: "image")
                 <*> ((o .:? "ports" .!= Array (fromList [])) >>= parseJSON)
        x -> fail $ "unexpected json: " ++ show x

data ContainerPort = ContainerPort
                   { port :: Int
                   , protocol :: Text
                   } deriving (Show)

instance FromJSON ContainerPort where
    parseJSON = \case
        Object o -> ContainerPort
                 <$> (o .: "containerPort")
                 <*> (o .: "protocol")
        x -> fail $ "unexpected json: " ++ show x

getDeployments :: IO (Either String [Deployment])
getDeployments = do
    r <- get $ deployments
    case decode (r ^. responseBody) of
        Just (DeploymentResult deps) -> return $ Right deps
        Nothing -> return $ Left $ "request kubernetes failed" ++ show (r ^. responseBody)

getDeploymentsOf :: String -> IO (Either String [Deployment])
getDeploymentsOf username = do
    r <- get $ deploymentsOf username
    case decode (r ^. responseBody) of
        Just (DeploymentResult deps) -> return $ Right deps
        Nothing -> return $ Left $ "request kubernetes failed" ++ show (r ^. responseBody)