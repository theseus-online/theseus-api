{-# LANGUAGE OverloadedStrings #-}

module  Kubernetes.Namespaces
    ( initNamespace
    ) where

import Kubernetes.Settings (namespaces)
import Network.Wreq (post)
import Data.Aeson ((.=), object)

initNamespace :: String -> IO (Either String ())
initNamespace namespace = do
    let n = object [ "metadata" .= object [ "name" .= namespace
                                          , "labels" .= object [ "namespace" .= namespace ]
                                          , "annotations" .= object [
                                                  "net.beta.kubernetes.io/network-policy" .=
                                                      ("{\"ingress\":{\"isolation\":\"DefaultDeny\"}}" :: String)]]]
    post namespaces n
    return $ Right ()
