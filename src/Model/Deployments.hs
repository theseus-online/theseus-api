{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Model.Deployments
    ( getDeployments
    , getDeploymentsOf
    , createDeployment
    , deleteDeployment
    , Deployment(..)
    ) where

import GHC.Generics (Generic)
import qualified Kubernetes.Deployments as KD
import qualified Kubernetes.Pods as KP
import qualified Kubernetes.NetworkPolicies as KN
import Data.Aeson (ToJSON(..), FromJSON, object, (.=))

data Deployment = Deployment
                { name :: String
                , owner :: String
                , replicas :: Int
                , containers :: [Container]
                , pods :: Maybe [Pod]          -- only in response
                } deriving (Show, Generic)

instance ToJSON Deployment
instance FromJSON Deployment

data Container = Container
               { name :: String
               , image :: String
               , volumes :: [Volume]
               , command :: Maybe String
               , args :: Maybe [String]
               } deriving (Show, Generic)

instance ToJSON Container
instance FromJSON Container

data Volume = Volume
            { name :: String
            , mountPath :: String
            } deriving (Show, Generic)

instance ToJSON Volume
instance FromJSON Volume

data Pod = Pod
         { name :: String
         , ip :: String
         , containers :: [ContainerInstance]
         } deriving (Show, Generic)

instance ToJSON Pod
instance FromJSON Pod

type ContainerInstance = KP.ContainerInstance

instance ToJSON ContainerInstance where
    toJSON (KP.ContainerInstance name status) = object ["name" .= name, "status" .= toJSON status]

type ContainerStatus = KP.ContainerStatus

instance ToJSON ContainerStatus where
    toJSON (KP.Running s) = object ["running" .= object["started" .= s]]
    toJSON (KP.Waiting r m) = object ["waiting" .= object["reason" .= r, "message" .= m]]
    toJSON (KP.Terminated s f e r m) = object ["terminated" .= object
        [ "started" .= s
        , "finished" .= f
        , "exitCode" .= e
        , "reason" .= r
        , "message" .= m
        ]]
    toJSON _ = "unknown"

fromKubeDeployment :: KD.Deployment -> Deployment
fromKubeDeployment (KD.Deployment nm ns rp cs) = Deployment nm ns rp (map fromKubeContainer cs) Nothing
    where
        fromKubeContainer (KD.Container nm im vs cm ags) =
            Container nm im (map fromKubeVolume vs) (cm >>= \(c:[]) -> Just c) ags
        fromKubeVolume (KD.Volume nm mp) = Volume nm mp

toKubeDeployment :: Deployment -> KD.Deployment
toKubeDeployment (Deployment nm ns rp cs _) = KD.Deployment nm ns rp $ map toKubeContainer cs
    where
        toKubeContainer (Container nm im vs cm ags) = KD.Container nm im (map toKubeVolume vs) (cm >>= \c -> Just [c]) ags
        toKubeVolume (Volume nm mp) = KD.Volume nm mp

fromKubePod :: KP.Pod -> Pod
fromKubePod (KP.Pod n _ _ ip cs) = Pod n ip cs

readDeploymentList :: IO (Either String [KD.Deployment])
                   -> IO (Either String [KP.Pod])
                   -> IO (Either String [Deployment])
readDeploymentList depsIO podsIO =
    depsIO >>= \case
        Left msg -> return $ Left msg
        Right deps -> do
            let ds = map (\dep -> fromKubeDeployment dep) deps
            podsIO >>= \case
                Left msg -> return $ Left msg
                Right pods -> return $ Right $ map (\d -> d { pods = Just $ filterPods d pods }) ds
    where
        filterPods (Deployment n _ _ _ _) ps =
            map (\(KP.Pod n _ _ ip cs) -> Pod n ip cs) (filter (\(KP.Pod _ a _ _ _) -> a == n) ps)


getDeployments :: IO (Either String [Deployment])
getDeployments = readDeploymentList KD.getDeployments KP.getPods

getDeploymentsOf :: String -> IO (Either String [Deployment])
getDeploymentsOf username = readDeploymentList (KD.getDeploymentsOf username) (KP.getPodsOf username)

createDeployment :: Deployment -> IO (Either String ())
createDeployment dep = do
    KN.createNetworkPolicy (owner dep) (name (dep :: Deployment))
    KD.createDeployment $ toKubeDeployment dep

deleteDeployment :: String -> String -> IO (Either String ())
deleteDeployment username name = do
    KN.deleteNetworkPolicy username name
    KD.deleteDeployment username name
