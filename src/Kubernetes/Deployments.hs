{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Kubernetes.Deployments 
    ( getDeployments
    , getDeploymentsOf
    , createDeployment
    , Deployment(Deployment)
    , Container(Container)
    ) where

import Data.Text (Text, unpack)
import GHC.Exts (fromList)
import Network.Wreq (get, post, responseBody, responseStatus, statusCode)
import Control.Lens ((^.), (^?), (^..))
import Kubernetes.Settings (deployments, deploymentsOf)
import Data.Aeson ((.:), (.!=), (.:?), (.=), encode, decode, object, FromJSON(..), Value(..))

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
               } deriving (Show)

instance FromJSON Container where
    parseJSON = \case
        Object o -> Container
                 <$> (o .: "name")
                 <*> (o .: "image")
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

createDeployment :: Deployment -> IO (Either String ())
createDeployment dep = do
    let d = object [ "metadata" .= name (dep :: Deployment)-- deploymentMeta dep
                   , "spec" .= object ["template" .= object ["spec" .= object ["containers" .= deploymentContainers dep]]]
                   ]
    r <- post ((deploymentsOf . unpack . namespace) dep) d
    case r ^. responseStatus . statusCode of
        200 -> return $ Right ()
        c -> return $ Left $ "kubernetes not response with 200. code: " ++ show c ++ ". body: " ++ show (r ^. responseBody)

    where deploymentMeta (Deployment nm ns _) = object [ "name" .= nm
                                                       , "namespace" .= ns
                                                       ]
          deploymentContainers (Deployment _ _ cs) = map (\(Container n i) -> object ["name" .= n, "image" .= i]) cs