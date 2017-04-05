{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kubernetes.Ingresses
    ( getIngresses
    , getIngressesOf
    , Ingress(..)
    ) where

import Data.Text (Text)
import Kubernetes.Settings (ingresses, ingressOf, ingressesOf)
import qualified Data.HashMap.Strict as HM
import GHC.Exts (fromList)
import Control.Lens ((&), (.~), (^.), (^?), (^..))
import Data.Aeson ((.:), (.!=), (.:?), (.=), encode, decode, object, FromJSON(..), Value(..))
import Network.Wreq (get, post, delete, deleteWith, param, defaults, responseBody, responseStatus, statusCode)

data IngressResult = IngressResult [Ingress] deriving (Show)

instance FromJSON IngressResult where
    parseJSON = \case
        Object o -> (o .: "items") >>= fmap IngressResult . parseJSON
        x -> fail $ "unexpected json: " ++ show x

data Ingress = Ingress
                           { name :: String
                           , namespace :: String
                           , host :: String
                           , secure :: Bool
                           , serviceName :: String
                           , servicePort :: Int
                            } deriving (Show)

instance FromJSON Ingress where
    parseJSON = \case
        Object o -> do
            name <- (o .: "metadata") >>= (.: "name")
            namespace <- (o .: "metadata") >>= (.: "namespace")
            host <- (o .: "spec") >>= (.: "rules") >>= ((.: "host") . head)
            secure <- (o .: "spec") >>= (\obj -> case HM.lookup ("tls" :: Text) obj of
                                                                            Just (_ :: Value) -> return True
                                                                            Nothing -> return False
                                                                        )
            rs <- (o .: "spec") >>= (.: "rules")
            let r = head rs
            ps <- (r .: "http") >>= (.: "paths")
            let p = head ps
            serviceName <- (p .: "backend") >>= (.: "serviceName")
            servicePort <- (p .: "backend") >>= (.: "servicePort")
            return $ Ingress name namespace host secure serviceName servicePort

getIngresses :: IO (Either String [Ingress])
getIngresses = do
    r <- get $ ingresses
    case decode (r ^. responseBody) of
        Just (IngressResult ings) -> return $ Right ings
        Nothing -> return $ Left $ "request kubernetes failed" ++ show (r ^. responseBody)

getIngressesOf :: String -> IO (Either String [Ingress])
getIngressesOf namespace = do
    r <- get $ ingressesOf namespace
    case decode (r ^. responseBody) of
        Just (IngressResult ings) -> return $ Right ings
        Nothing -> return $ Left $ "request kubernetes failed" ++ show (r ^. responseBody)
