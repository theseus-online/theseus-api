{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kubernetes.Ingresses
    ( getIngresses
    , getIngressesOf
    , createIngress
    , Ingress(..)
    ) where

import qualified Data.Text as T
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
            secure <- (o .: "spec") >>= (\obj -> case HM.lookup ("tls" :: T.Text) obj of
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

createIngress :: Ingress -> IO (Either String ())
createIngress ing = do
    let i = object [ "metadata" .= ingressMeta ing
                   , "spec" .= ingressSpec ing
                   ]
    post ((ingressesOf . namespace) ing) i
    return $ Right ()

    where ingressMeta (Ingress nm ns _ s _ _) = object [ "name" .= nm
                                                       , "namespace" .= ns
                                                       , "annotations" .= secureAnnotation s
                                                       ]

          secureAnnotation secure =
              if secure then
                  object [ "ingress.kubernetes.io/ssl-passthrough" .= ("true" :: T.Text)
                         , "ingress.kubernetes.io/ssl-redirect" .= ("true" :: T.Text)
                         ]
              else
                  object []

          ingressSpec (Ingress _ ns h s sn sp) =
              if s then
                  object [ "tls" .= tlsSpec h
                         , "rules" .= rulesSpec h sn sp
                         ]
              else
                  object [ "rules" .= rulesSpec h sn sp ]

          tlsSpec h = Array $ fromList [ object [ "hosts" .= Array (fromList [String . T.pack $ h]) ] ]

          rulesSpec h sn sp = Array $ fromList [ object [ "host" .= h
                                                        , "http" .= object [ "paths" .= pathsSpec sn sp ]
                                                        ]]

          pathsSpec sn sp = Array $ fromList [ object [ "backend" .= object [ "serviceName" .= sn
                                                                            , "servicePort" .= sp
                                                                            ]]]
