module Kubernetes.Settings
    ( deployments
    , deploymentsOf
    ) where

apiV1 :: String
apiV1 = "http://theseus:8080/api/v1"

apiExtensions :: String
apiExtensions = "http://theseus:8080/apis/extensions/v1beta1"

deployments :: String
deployments = apiExtensions ++ "/deployments"

deploymentsOf :: String -> String
deploymentsOf username = apiExtensions ++ "/namespaces/" ++ username ++ "/deployments"