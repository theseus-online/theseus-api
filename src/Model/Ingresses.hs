{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Model.Ingresses
    ( getIngresses
    , getIngressesOf
    , createIngress
    , deleteIngress
    , Ingress(..)
    ) where

import GHC.Generics (Generic)
import qualified Kubernetes.Ingresses as KI
import Data.Aeson (ToJSON, FromJSON)

data Ingress = Ingress
             { name :: String
             , owner :: String
             , host :: String
             , secure :: Bool
             , serviceName :: String
             , servicePort :: Int
             } deriving (Show, Generic)

instance FromJSON Ingress
instance ToJSON Ingress

fromKubeIngress ::  KI.Ingress -> Ingress
fromKubeIngress (KI.Ingress nm hs h s sn sp)= Ingress nm hs h s sn sp

toKubeIngress :: Ingress -> KI.Ingress
toKubeIngress (Ingress nm hs h s sn sp) = KI.Ingress nm hs h s sn sp

getIngresses :: IO (Either String [Ingress])
getIngresses = KI.getIngresses >>= \case
    Left msg -> return $ Left msg
    Right ings -> return $ Right $ map (\ing -> fromKubeIngress ing) ings

getIngressesOf :: String -> IO (Either String [Ingress])
getIngressesOf username = KI.getIngressesOf username >>= \case
    Left msg -> return $ Left msg
    Right ings -> return $ Right $ map (\ing -> fromKubeIngress ing) ings

createIngress :: Ingress -> IO (Either String ())
createIngress ing = KI.createIngress $ toKubeIngress ing

deleteIngress :: String -> String -> IO (Either String ())
deleteIngress = KI.deleteIngress
