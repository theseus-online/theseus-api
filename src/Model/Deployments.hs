{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Model.Deployments
    ( getDeployments
    , getDeploymentsOf
    , createDeployment
    , Deployment(..)
    ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Kubernetes.Deployments as KD
import Data.Aeson (ToJSON, FromJSON)

data Deployment = Deployment
                { name :: Text
                , owner :: Text
                , containers :: [Container]
                } deriving (Show, Generic)

instance ToJSON Deployment
instance FromJSON Deployment

data Container = Container
               { name :: Text
               , image :: Text
               } deriving (Show, Generic)

instance ToJSON Container
instance FromJSON Container

fromKubeDeployment :: KD.Deployment -> Deployment
fromKubeDeployment (KD.Deployment nm ns cs) = Deployment nm ns $ map fromKubeContainers cs
    where fromKubeContainers (KD.Container nm im) = Container nm im

toKubeDeployment :: Deployment -> KD.Deployment
toKubeDeployment (Deployment nm ns cs) = KD.Deployment nm ns $ map toKubeContainers cs
    where toKubeContainers (Container nm im) = KD.Container nm im

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