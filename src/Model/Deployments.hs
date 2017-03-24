{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Model.Deployments
    ( getDeployments
    , getDeploymentsOf
    , Deployment
    ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Kubernetes.Deployments as KD
import Data.Aeson (ToJSON)

data Deployment = Deployment
                { name :: Text
                , owner :: Text
                , containers :: [Container]
                } deriving (Show, Generic)

instance ToJSON Deployment

data Container = Container
               { name :: Text
               , image :: Text
               } deriving (Show, Generic)

instance ToJSON Container

fromKubeDeployment :: KD.Deployment -> Deployment
fromKubeDeployment (KD.Deployment nm ns cs) = Deployment nm ns $ foldr foldContainers [] cs
    where foldContainers (KD.Container nm im _) list = (Container nm im) : list

getDeployments :: IO (Either String [Deployment])
getDeployments =
    KD.getDeployments >>= \case
        Left msg -> return $ Left msg
        Right deps -> return $ Right $ foldr (\dep list -> (fromKubeDeployment dep) : list) [] deps

getDeploymentsOf :: String -> IO (Either String [Deployment])
getDeploymentsOf username =
    KD.getDeploymentsOf username >>= \case
        Left msg -> return $ Left msg
        Right deps -> return $ Right $ foldr (\dep list -> (fromKubeDeployment dep) : list) [] deps