{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Model.Services 
    ( getServices
    , getServicesOf
    , createService
    , Service(..)
    ) where

import GHC.Generics (Generic)
import qualified Kubernetes.Services as KS
import Data.Aeson (ToJSON, FromJSON)

data Service = Service
             { name :: String
             , owner :: String
             , backend :: String
             , ports :: [Port]
             } deriving (Show, Generic)

instance ToJSON Service
instance FromJSON Service

data Port = Port
          { name :: String
          , protocol :: String
          , port :: Int
          , targetPort :: Int
          } deriving (Show, Generic)

instance ToJSON Port
instance FromJSON Port

fromKubeService :: KS.Service-> Service
fromKubeService (KS.Service nm ns be ps) = Service nm ns be $ map fromKubePort ps
    where fromKubePort(KS.Port nm pro p tp) = Port nm pro p tp

toKubeService :: Service -> KS.Service
toKubeService (Service nm ns be ps) = KS.Service nm ns be $ map toKubePort ps
    where toKubePort (Port nm pro p tp) = KS.Port nm pro p tp

getServices :: IO (Either String [Service])
getServices = 
    KS.getServices >>= \case
        Left msg -> return $ Left msg
        Right svcs -> return $ Right $ map (\svc -> fromKubeService svc) svcs

getServicesOf :: String -> IO (Either String [Service])
getServicesOf username = 
    KS.getServicesOf username >>= \case
        Left msg -> return $ Left msg
        Right svcs -> return $ Right $ map (\svc -> fromKubeService svc) svcs

createService :: Service -> IO (Either String ())
createService svc = KS.createService $ toKubeService svc