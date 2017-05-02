{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Kubernetes.Services
    ( getServices
    , getServicesOf
    , createService
    , deleteService
    , Service(Service)
    , Port(Port)
    ) where

import GHC.Exts (fromList)
import Kubernetes.Settings (services, serviceOf, servicesOf)
import Control.Lens ((&), (.~), (^.), (^?), (^..))
import Data.Aeson ((.:), (.!=), (.:?), (.=), encode, decode, object, FromJSON(..), Value(..))
import Network.Wreq (get, post, delete, deleteWith, param, defaults, responseBody, responseStatus, statusCode)

data ServiceResult = ServiceResult [Service] deriving (Show)

instance FromJSON ServiceResult where
    parseJSON = \case
        Object o -> (o .: "items") >>= fmap ServiceResult . parseJSON
        x -> fail $ "unexpected json: " ++ show x

data Service = Service
             { name :: String
             , namespace :: String
             , backend :: String
             , ports :: [Port]
             } deriving (Show)

instance FromJSON Service where
    parseJSON = \case
        Object o -> Service
                <$> ((o .: "metadata") >>= (.: "name"))
                <*> ((o .: "metadata") >>= (.: "namespace"))
                <*> ((((o .: "spec") >>= (.:? "selector")) .!= (fromList []) >>= (.:? "app")) .!= "<backend>")
                <*> ((o .: "spec") >>= (.: "ports") >>= parseJSON)
        x -> fail $ "unexpected json: " ++ show x

data Port = Port
          { name :: String
          , protocol :: String
          , port :: Int
          , targetPort :: Int
          } deriving (Show)

instance FromJSON Port where
    parseJSON = \case
        Object o -> Port
                <$> (o .:? "name" .!= "")
                <*> (o .: "protocol")
                <*> (o .: "port")
                <*> (o .: "targetPort")
        x -> fail $ "unexpected json: " ++ show x

getServices :: IO (Either String [Service])
getServices = do
    r <- get $ services
    case decode (r ^. responseBody) of
        Just (ServiceResult svcs) -> return $ Right svcs
        Nothing -> return $ Left $ "request kubernetes failed" ++ show (r ^. responseBody)

getServicesOf :: String -> IO (Either String [Service])
getServicesOf namespace = do
    r <- get $ servicesOf namespace
    case decode (r ^. responseBody) of
        Just (ServiceResult svcs) -> return $ Right svcs
        Nothing -> return $ Left $ "request kubernetes failed" ++ show (r ^. responseBody)

createService :: Service -> IO (Either String ())
createService svc = do
    let s = object [ "metadata" .= serviceMeta svc
                   , "spec" .= serviceSpec svc
                   ]
    post ((servicesOf . namespace) svc) s
    return $ Right ()

    where serviceMeta (Service nm ns _ _) = object [ "name" .= nm
                                                    , "namespace" .= ns
                                                    , "labels" .= object ["service" .= nm]
                                                    ]

          serviceSpec (Service _ _ be ps) = object [ "selector" .= object ["app" .= be]
                                                   , "ports" .= servicePorts ps
                                                   ]

          servicePorts ps = map (\(Port nm pro p tp) -> object ["name" .= nm, "protocol" .= pro, "port" .= p, "targetPort" .= tp]) ps

deleteService :: String -> String -> IO (Either String ())
deleteService namespace name = do
    delete $ serviceOf namespace name
    return $ Right ()
