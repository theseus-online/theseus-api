{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Aeson (ToJSON, FromJSON)

data Deployment = Deployment
                { name :: String
                , owner :: String
                , containers :: [Container]
                } deriving (Show, Generic)

instance ToJSON Deployment
instance FromJSON Deployment

data Container = Container
               { name :: String
               , image :: String
               , volumes :: [Volume]
               } deriving (Show, Generic)

instance ToJSON Container
instance FromJSON Container

data Volume = Volume
            { name :: String
            , mountPath :: String
            } deriving (Show, Generic)

instance ToJSON Volume
instance FromJSON Volume

fromKubeDeployment :: KD.Deployment -> Deployment
fromKubeDeployment (KD.Deployment nm ns cs) = Deployment nm ns $ map fromKubeContainer cs
    where
        fromKubeContainer (KD.Container nm im vs) = Container nm im $ map fromKubeVolume vs
        fromKubeVolume (KD.Volume nm mp) = Volume nm mp

toKubeDeployment :: Deployment -> KD.Deployment
toKubeDeployment (Deployment nm ns cs) = KD.Deployment nm ns $ map toKubeContainer cs
    where
        toKubeContainer (Container nm im vs) = KD.Container nm im $ map toKubeVolume vs
        toKubeVolume (Volume nm mp) = KD.Volume nm mp

getDeployments :: IO (Either String [Deployment])
getDeployments =
    KD.getDeployments >>= \case
        Left msg -> return $ Left msg
        Right deps -> return $ Right $ map (\dep -> fromKubeDeployment dep) deps

getDeploymentsOf :: String -> IO (Either String [Deployment])
getDeploymentsOf username =
    KD.getDeploymentsOf username >>= \case
        Left msg -> return $ Left msg
        Right deps -> return $ Right $ map (\dep -> fromKubeDeployment dep) deps

createDeployment :: Deployment -> IO (Either String ())
createDeployment dep = KD.createDeployment $ toKubeDeployment dep

deleteDeployment :: String -> String -> IO (Either String ())
deleteDeployment = KD.deleteDeployment
