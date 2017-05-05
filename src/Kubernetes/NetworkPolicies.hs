{-# LANGUAGE OverloadedStrings #-}

module  Kubernetes.NetworkPolicies
    ( createNetworkPolicy
    , deleteNetworkPolicy
    ) where

import Kubernetes.Settings (networkPolicyOf, networkPoliciesOf)
import Network.Wreq (post, delete)
import Data.Aeson ((.=), object, Value(..))
import GHC.Exts (fromList)

createNetworkPolicy :: String -> String -> IO (Either String ())
createNetworkPolicy namespace name = do
    let p = object [ "metadata" .= object [ "name" .= name, "namespace" .= namespace]
                   , "spec" .= object [ "podSelector" .= podSelector
                                      , "ingress" .= ingress
                                      ]]
    post (networkPoliciesOf namespace) p
    return $ Right ()

    where
        podSelector = object [ "matchLabels" .= object [ "app" .= name ] ]
        ingress = Array $ fromList [ object [ "from" .= fromSelectors ] ]
        fromSelectors =
            Array $ fromList [ object [ "namespaceSelector" .=
                                 object [ "matchLabels" .= object [ "role" .= ("system" :: String) ] ]
                               ]
                             , object [ "podSelector" .= object [] ]    -- all pods in current namespace
                             ]

deleteNetworkPolicy :: String -> String -> IO (Either String ())
deleteNetworkPolicy namespace name = do
    delete $ networkPolicyOf namespace name
    return $ Right ()
